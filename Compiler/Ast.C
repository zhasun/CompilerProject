#include "Ast.h"
#include "ParserUtil.h"
#include "MemoryMgr.h"
#include "ICodeValue.h"


AstNode::AstNode(NodeType nt, int line, int column, string file):
  ProgramElem(NULL, line, column, file) {
	nodeType_ = nt;
}

AstNode::AstNode(const AstNode& ast): ProgramElem(ast) {
	nodeType_ = ast.nodeType();
}

/****************************************************************/

ExprNode::ExprNode(ExprNodeType et, const Value* val, int line, int column, 
				   string file):
	AstNode(AstNode::NodeType::EXPR_NODE, line, column, file)
{
	exprType_ = et;
  val_ = val;
  register_ = Register();
  temp_reg_ = Register();
  startLabel(NULL);
  trueLabel(NULL);
  falseLabel(NULL);
  endLabel(NULL);
}

ExprNode::ExprNode(const ExprNode& e) : AstNode(e)
{
	exprType_ = e.exprNodeType();
  val_ = e.value();
  register_ = e.reg();
  temp_reg_ = e.tempReg();
  startLabel(e.startLabel());
  trueLabel(e.trueLabel());
  falseLabel(e.falseLabel());
  endLabel(e.endLabel());
}

RefExprNode::RefExprNode(string ext, const SymTabEntry* ste, int line, int column, string file) :
  ExprNode(ExprNode::ExprNodeType::REF_EXPR_NODE,NULL,line,column,file) {
  ext_ = ext;
  if(ste == NULL){
    sym_ = stm.lookUp(ext_);
    if(sym_ == NULL){
      errMsg("Undeclared identifier \"" + ext_ + "\"");
    }
  } else {
    sym_ = ste;
  }
  if(sym_!=NULL){
    type(sym_->type());
  } else {
    type(new Type(Type::TypeTag::ERROR));
  }
}

RefExprNode::RefExprNode(const RefExprNode& ren) : 
  ExprNode(ren){
  ext_ = ren.ext_;
  sym_ = ren.sym_;
}

void RefExprNode::print(ostream& os, int indent) const {
  os << ext_ << "(offset_ = " << offSet() << ", from_bp_ = " << fromBP();
  if(reg().isValid()){
    os << ", ref_register_ = " << reg();
  }
  os << ")";
}

void RefExprNode::memAlloc(MemoryMgr &mm){
  if(sym_ != NULL && sym_->kind() == SymTabEntry::Kind::VARIABLE_KIND){
    VariableEntry *ve = (VariableEntry*) sym_;
    if(ve->varKind() == VariableEntry::VarKind::GLOBAL_VAR){
      fromBP(false);
      offSet(ve->offSet());
    } else if(ve->varKind() == VariableEntry::VarKind::PARAM_VAR){
      fromBP(true);
      offSet(ve->offSet());
    } else if(ve->varKind() == VariableEntry::VarKind::LOCAL_VAR){
      fromBP(true);
      offSet(ve->offSet());
    }    
    mem_register_ = mm.getNextRegister(true);
    mm.addRegister(mem_register_);
    if(Register::sameRegisters(type(),coercedType())){
      reg(mm.getNextRegister(type()));
      mm.addRegister(reg());
    } else {
      tempReg(mm.getNextRegister(type()));
      mm.addRegister(tempReg());
      reg(mm.getNextRegister(coercedType()));
      mm.addRegister(reg());
      mm.freeRegister(tempReg());
    }
    mm.freeRegister(mem_register_);
  }
}

void RefExprNode::lhsMemAlloc(MemoryMgr &mm){
  if(sym_ != NULL && sym_->kind() == SymTabEntry::Kind::VARIABLE_KIND){
    VariableEntry *ve = (VariableEntry*) sym_;
    if(ve->varKind() == VariableEntry::VarKind::GLOBAL_VAR){
      fromBP(false);
      offSet(ve->offSet());
    } else if(ve->varKind() == VariableEntry::VarKind::PARAM_VAR){
      fromBP(true);
      offSet(ve->offSet());
    } else if(ve->varKind() == VariableEntry::VarKind::LOCAL_VAR){
      fromBP(true);
      offSet(ve->offSet());
    }
    memReg(mm.getNextRegister(true));
    mm.addRegister(memReg());
    mm.freeRegister(memReg());
  }
}

void RefExprNode::typePrint(ostream& os, int indent) const {
  if(coercedType() != NULL){
    os << "(" << coercedType()->fullName() << ")";
  }
  os << type()->fullName();
}

CodeBlock* RefExprNode::codeGen(){
  //Computes address of variable
  CodeBlock* r_block = new CodeBlock();

  if(startLabel()!=NULL){
    r_block->setStartLabel(startLabel());
  }

  r_block->append(getAddrCodeGen());

  //Load value
  ICode::ICodeType ict = ICode::ICodeType::LDI;
  if(destReg().isFloat()){
    ict = ICode::ICodeType::LDF;
  }

  // Move value at address in register into the register (should maybe use two different registers)
  ICode mov = ICode(ict,&mem_register_,&destReg());

  r_block->append(mov);

  if(type()->tag() == Type::TypeTag::BOOL){
    ICode* cmp_true = new ICode(ICode::ICodeType::NE,new Value(0,Type::TypeTag::UINT),&reg());
    ICode jmp_true(ICode::ICodeType::JMPC,cmp_true,trueLabel());
    ICode jmp_false(ICode::ICodeType::JMP,falseLabel());
    r_block->append(jmp_true);
    r_block->append(jmp_false);
  }

  if(!Register::sameRegisters(type(),coercedType())){
      ICode::ICodeType mov_type = ICode::getMovType(tempReg(),reg());
      ICode coerce_stmt = ICode(mov_type,&tempReg(),&reg());  
      r_block->append(coerce_stmt);
  }

  return r_block;
}

CodeBlock* RefExprNode::getAddrCodeGen(){
  ICode la;
  if(fromBP()){
    la = ICode(ICode::ICodeType::ADD,&(MemoryMgr::basePointerRegister()),new Value(offSet(),Type::TypeTag::UINT),&(memReg()));
  } else {
    la = ICode(ICode::ICodeType::MOVI,new Value(offSet(),Type::TypeTag::UINT),&(memReg()));
  }
  CodeBlock* r_block = new CodeBlock();

  r_block->append(la);

  return r_block;
}

/****************************************************************/
extern const OpNode::OpInfo opInfo[] = {
  // print name, arity, paren_flag, fixity, arg types, out type, constraints
  //
  // Paren_flag -- opnode->print() outputs is surrounded by parenthesis if 
  // this flag is set. As set below, the expression may not print correctly
  // in some rare cases, e.g., ~(b * c) will get printed as ~b * c,
  // which actually corresponds to (~b)*c. To ensure that things get printed
  // correctly all the time, more paren_flags should be set to 1, but this
  // will lead to more clutter in printed output. Basically, what we have done
  // here is to look are expressions by type -- arithmetic, relational, 
  // boolean, bit operations, etc. Within each type, the highest priority 
  // operator is printed without paren. This will work correctly, as long
  // as the language doesn't permit mixing of different types of expressions.
  // But this assumption doesn't always hold, as in the example above. Also,
  // there is an exception to this general approach in the case of unary minus
  // and * -- since (-a)*b and -(a*b) have the same meaning, we can exclude
  // paren for * without an error.
  //
  // Codes for constraints:
  // first character:
  //    N: No additional constraint over what is given by argTypes
  //    I: all arguments must have identical type
  //    S: one of the arguments must have a type that is a supertype of
  //        of all other arguments. All other arguments require a coercion 
  //        operation to be introduced so as to convert their type to S.
  //    s: one of the arguments must have a type that is a subtype of
  //        of all other arguments. 
  //    L: all list arguments (and list output) must have same type. In 
  //        addition, all non-list arguments (and output) must have same 
  //        type as that of elements in these lists
  //    T: all tuple arguments to the function must have same type.
  //    A: (assignment). Type of second argument must be a subtype of
  //       the first argument
  //
  // second character:
  //    O: output type is the same as out type. (In the following cases,
  //        the output type need not be equal to out type, but a subtype
  //        of it.) Since a TypeTag provides complete type information only
  //        for primitive types, `O' is applicable only in this case.
  //    digit: output type is the same as that of the digit'th argument
  //       In this case, a third character may be used, the code for
  //       which is as follows:
  //         'e' denotes that the output is of type alpha, where
  //             the type of digit'th argument is list(alpha)
  //         'l' denotes that the output is of type list(alpha), where
  //             alpha is the type of the digit'th argument.
  //    S: The output type is the same as that of the argument with the
  //        most general type. (Typically used with first character 'S')
  //    s: The output type is the same as that of the argument with the
  //        least general type. (Typically used with first character 'S')
  //    P: The output type is the product of the types of all arguments
  //    p: The output type is a component of the input tuple type. The
  //        following character specifies the component. A digit k specifies
  //        that the component number as k. The character 'a' indicates that
  //        the component number is given by an integer argument to the
  //        operator. The argument number is given by the following digit.
  //        'p' can be used only in conjunction with first character 'P'.
  //    L: Output type is the same as type of list arguments. Can be used
  //        only in conjunction with first character L.
  //    e: Output type is the same as type of element of list arguments. 
  //        Can be used only in conjunction with first character L.
  //
  {OpNode::OpCode::UMINUS, "-",  1, 0, OpNode::OpPrintType::PREFIX, {Type::SIGNED}, Type::SIGNED, "N1" , ICode::ICodeType::NEG , ICode::ICodeType::FNEG},
  {OpNode::OpCode::PLUS, "+",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS" , ICode::ICodeType::ADD , ICode::ICodeType::FADD},
  {OpNode::OpCode::MINUS, "-",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS" , ICode::ICodeType::SUB , ICode::ICodeType::FSUB},
  {OpNode::OpCode::MULT, "*",  2, 0, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS" , ICode::ICodeType::MUL , ICode::ICodeType::FMUL},
  {OpNode::OpCode::DIV, "/",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS" , ICode::ICodeType::DIV , ICode::ICodeType::FDIV},
  {OpNode::OpCode::MOD, "%",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "S2" , ICode::ICodeType::MOD , ICode::ICodeType::MOD},
  {OpNode::OpCode::EQ, "==", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO" , ICode::ICodeType::EQ , ICode::ICodeType::FEQ},
  {OpNode::OpCode::NE, "!=", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO" , ICode::ICodeType::NE , ICode::ICodeType::FNE},
  {OpNode::OpCode::GT, ">",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO" , ICode::ICodeType::GT , ICode::ICodeType::FGT},
  {OpNode::OpCode::LT, "<",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO" , ICode::ICodeType::GT , ICode::ICodeType::FGT},
  {OpNode::OpCode::GE, ">=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO" , ICode::ICodeType::GE , ICode::ICodeType::FGE},
  {OpNode::OpCode::LE, "<=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO" , ICode::ICodeType::GE , ICode::ICodeType::FGE},
  {OpNode::OpCode::AND, "&&",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO" , ICode::ICodeType::AND , ICode::ICodeType::AND},
  {OpNode::OpCode::OR, "||",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO" , ICode::ICodeType::OR , ICode::ICodeType::OR},
  {OpNode::OpCode::NOT, "!",  1, 0, OpNode::OpPrintType::PREFIX, {Type::BOOL}, Type::BOOL, "NO" , ICode::ICodeType::XOR , ICode::ICodeType::XOR},
  {OpNode::OpCode::BITNOT, "~",  1, 0, OpNode::OpPrintType::PREFIX, {Type::INTEGRAL}, Type::INTEGRAL, "N1" , ICode::ICodeType::XOR , ICode::ICodeType::XOR},
  {OpNode::OpCode::BITAND, "&",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "Ss" , ICode::ICodeType::AND , ICode::ICodeType::AND},
  {OpNode::OpCode::BITOR, "|",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS" , ICode::ICodeType::OR , ICode::ICodeType::OR},
  {OpNode::OpCode::BITXOR, "^",  2, 0, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS" , ICode::ICodeType::XOR , ICode::ICodeType::XOR},
  {OpNode::OpCode::SHL, "<<", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1" , ICode::ICodeType::MUL , ICode::ICodeType::FMUL},
  {OpNode::OpCode::SHR, ">>", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1" , ICode::ICodeType::DIV , ICode::ICodeType::FDIV},
  {OpNode::OpCode::ASSIGN, "=",  2, 0, OpNode::OpPrintType::INFIX, {Type::NATIVE, Type::NATIVE}, Type::VOID, "AO" , ICode::ICodeType::MOVI , ICode::ICodeType::MOVF},
  {OpNode::OpCode::PRINT, "print", OpNode::VARIABLE, 1, OpNode::OpPrintType::PREFIX, {Type::NATIVE}, Type::VOID, "NO" , ICode::ICodeType::PRTI , ICode::ICodeType::PRTF},
  {OpNode::OpCode::INVALID, "invalid",            0, 0, OpNode::OpPrintType::PREFIX, {}, Type::ERROR, "NO"}
};

OpNode::OpNode(OpCode op, ExprNode* a1, ExprNode* a2, 
			   int ln, int col, string file):
  ExprNode(ExprNode::ExprNodeType::OP_NODE, NULL, ln,col,file) {
  opCode_ = op;
  if (a1 != NULL) {
	arity_ = 1;
	arg_.push_back(a1);
	if (a2 != NULL) {
	  arity_++;
	  arg_.push_back(a2);
	}
  }
}

OpNode::OpNode(const OpNode &other):
  ExprNode(other) {
  arity_ = other.arity();
  opCode_ = other.opCode();
  for (unsigned int i=0; (i < other.arity()); i++) {
    if (other.arg_[i]) {
      arg_.push_back((other.arg_[i])->clone());
    } 
	else {
      arg_.push_back(NULL);
    }
  }
}

void OpNode::print(ostream& os, int indent) const {
	int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
	os << opInfo[iopcode].name_;
	if (arity_ > 0) {
	  if (opInfo[iopcode].needParen_) 
		os << '(';
	  for (unsigned i=0; i < arity_-1; i++) {
		if (arg_[i])
		  arg_[i]->print(os, indent);
	    else os << "NULL";
		os << ", ";
	  }
      if (arg_[arity_-1])
		arg_[arity_-1]->print(os, indent);
	  else os << "NULL";
	  if (opInfo[iopcode].needParen_) 
		os << ") ";
	}
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
	if (opInfo[iopcode].needParen_) 
	  os << "(";
	if(arg_[0])
	  arg_[0]->print(os, indent);
	else os << "NULL";
	os << opInfo[iopcode].name_; 
	if(arg_[1])
	  arg_[1]->print(os, indent);
	else os << "NULL";
	if (opInfo[iopcode].needParen_) 
	  os << ")";
  }
  else internalErr("Unhandled case in OpNode::print");

  if(reg().isValid()){
    os << "(op_register_ = " << reg() << ")";
  }
}

void OpNode::typePrint(ostream& os, int indent) const {

  if(coercedType() != NULL){
    os << "(" << coercedType()->fullName() << ")";
  }

  int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
  os << opInfo[iopcode].name_;
  if (arity_ > 0) {
    if (opInfo[iopcode].needParen_) 
    os << '(';
    for (unsigned i=0; i < arity_-1; i++) {
    if (arg_[i])
      arg_[i]->typePrint(os, indent);
      else os << "NULL";
    os << ", ";
    }
      if (arg_[arity_-1])
    arg_[arity_-1]->typePrint(os, indent);
    else os << "NULL";
    if (opInfo[iopcode].needParen_) 
    os << ") ";
  }
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
  if (opInfo[iopcode].needParen_) 
    os << "(";
  if(arg_[0])
    arg_[0]->typePrint(os, indent);
  else os << "NULL";
  os << opInfo[iopcode].name_; 
  if(arg_[1])
    arg_[1]->typePrint(os, indent);
  else os << "NULL";
  if (opInfo[iopcode].needParen_) 
    os << ")";
  }
  else internalErr("Unhandled case in OpNode::print");
}
 
void OpNode::memAlloc(MemoryMgr &mm){
  if(isAssign()){
    if(isBoolAssign()){
      trueLabel(new Label(Label::LabelType::BOOL_TRUE));
      falseLabel(new Label(Label::LabelType::BOOL_FALSE));
      endLabel(new Label(Label::LabelType::BOOL_END));
      arg(1)->trueLabel(trueLabel());
      arg(1)->falseLabel(falseLabel());
    }
    RefExprNode* lhs = (RefExprNode*) arg(0);
    arg(1)->memAlloc(mm);
    lhs->lhsMemAlloc(mm);
    mm.freeRegister(arg(1)->reg());
  } else if(isRelational()){
    for(unsigned int i = 0; i < arity(); ++i){
      if(arg(i)!=NULL){
        arg(i)->memAlloc(mm);
      }
    }
    for(unsigned int i = 0; i < arity(); ++i){
      mm.freeRegister(arg(i)->reg());
    }
  } else if(isLogical()){
    if(opCode() == OpCode::AND){
      arg(1)->startLabel(new Label(Label::LabelType::BOOL_START));

      arg(0)->trueLabel(arg(1)->startLabel());
      arg(0)->falseLabel(falseLabel());

      arg(1)->trueLabel(trueLabel());
      arg(1)->falseLabel(falseLabel());
    }
    if(opCode() == OpCode::OR){
      arg(1)->startLabel(new Label(Label::LabelType::BOOL_START));

      arg(0)->trueLabel(trueLabel());
      arg(0)->falseLabel(arg(1)->startLabel());

      arg(1)->trueLabel(trueLabel());
      arg(1)->falseLabel(falseLabel());
    }
    if(opCode() == OpCode::NOT){
      arg(0)->trueLabel(falseLabel());
      arg(0)->falseLabel(trueLabel());
    }
    for(unsigned int i = 0; i < arity(); ++i){
      if(arg(i)!=NULL){
        arg(i)->memAlloc(mm);
      }
    }
  } else {
    for(unsigned int i = 0; i < arity(); ++i){
      if(arg(i)!=NULL){
        arg(i)->memAlloc(mm);
      }
    }
    for(unsigned int i = 0; i < arity(); ++i){
      mm.freeRegister(arg(i)->reg());
    }
    if(Register::sameRegisters(type(),coercedType())){
      reg(mm.getNextRegister(type()));
      mm.addRegister(reg());
    } else {
      tempReg(mm.getNextRegister(type()));
      mm.addRegister(reg());
      reg(mm.getNextRegister(coercedType()));
      mm.addRegister(reg());
      mm.freeRegister(tempReg());
    }
  }
}

CodeBlock* OpNode::codeGen() {
  CodeBlock* op_block = new CodeBlock();
  if(startLabel()!=NULL){
    op_block->setStartLabel(startLabel());
  }
  if(!isAssign()){
    for(unsigned int i = 0; i < arity(); ++i){
      if(arg(i)!=NULL){
        op_block->append(arg(i)->codeGen());
      }
    }
  }
  

  int iopcode = static_cast<int>(opCode_);
  ICode::ICodeType op_type;
  if(destReg().isInt()){
    op_type = opInfo[iopcode].int_icodeType_;
  } else {
    op_type = opInfo[iopcode].float_icodeType_;
  }

  CodeBlock* expr_block = new CodeBlock();
  if(isArithmetic()){
    ICode expr_stmt;
    if(arity() == 1){
      expr_stmt = ICode(op_type,&arg(0)->reg(),&destReg());
    }
    if(arity() == 2){
      expr_stmt = ICode(op_type,&arg(0)->reg(),&arg(1)->reg(),&destReg());
    }
    expr_block->append(expr_stmt);
  }
  if(isBitWise()){
    if(opCode() == OpCode::BITNOT){
      ICode bitnot_stmt(ICode::ICodeType::XOR,&arg(0)->reg(),new Value(~0,Type::TypeTag::UINT),&destReg());
      expr_block->append(bitnot_stmt);
    }
    if(opCode() == OpCode::BITAND || opCode() == OpCode::BITOR || opCode() == OpCode::BITXOR){
      ICode bit_stmt(opInfo[iopcode].int_icodeType_,&arg(0)->reg(),&arg(1)->reg(),&destReg());
      expr_block->append(bit_stmt);
    }
    if(opCode() == OpCode::SHL){
      //Not sure what to do here
    }
    if(opCode() == OpCode::SHR){
      //Or here
    }
  }
  if(isRelational()){
    int r_arg = 0, l_arg = 1;
    if(opCode() == OpCode::LT || opCode() == OpCode::LE){
      r_arg = 1;
      l_arg = 0;
    }
    if(arg(r_arg)->reg().isInt()){
      op_type = opInfo[iopcode].int_icodeType_;
    } else {
      op_type = opInfo[iopcode].float_icodeType_;
    }
    ICode* condition = new ICode(op_type,&arg(r_arg)->reg(),&arg(l_arg)->reg());
    ICode cond_jmp(ICode::ICodeType::JMPC,condition,trueLabel());
    ICode jmp(ICode::ICodeType::JMP,falseLabel());
    op_block->append(cond_jmp);
    op_block->append(jmp);
  }
  if(isLogical()){
    // Nothing is done, because shortcircuiting is done in the memalloc phase
  }
  if(isAssign()){
    RefExprNode* re = (RefExprNode*)arg(0);

    //Compute rhs
    op_block->append(arg(1)->codeGen());

    
    if(isBoolAssign()){
      CodeBlock* true_block = new CodeBlock();

      ICode store_true_val(ICode::ICodeType::STI,new Value(1,Type::TypeTag::UINT),&re->memReg());
      ICode jmp_end(ICode::ICodeType::JMP,endLabel());

      true_block->setStartLabel(trueLabel());
      true_block->append(re->getAddrCodeGen());
      true_block->append(store_true_val);
      true_block->append(jmp_end);

      CodeBlock* false_block = new CodeBlock();

      ICode store_false_val(ICode::ICodeType::STI,new Value(0,Type::TypeTag::UINT),&re->memReg());

      false_block->setStartLabel(falseLabel());
      false_block->append(re->getAddrCodeGen());
      false_block->append(store_false_val);

      op_block->append(true_block);
      op_block->append(false_block);
      op_block->setEndLabel(endLabel());
    } else {
      //Get address of lhs
      op_block->append(re->getAddrCodeGen());

      ICode::ICodeType store_type = ICode::ICodeType::STI;
      if(arg(1)->reg().isFloat()){
        store_type = ICode::ICodeType::STF;
      }

      ICode store_val(store_type,&arg(1)->reg(),&re->memReg());
      op_block->append(store_val);
    }
  }
  if(isArithmetic() || isBitWise()){
    if(!Register::sameRegisters(type(),coercedType())){
      ICode::ICodeType mov_type = ICode::getMovType(tempReg(),reg());
      ICode coerce_stmt = ICode(mov_type,&tempReg(),&reg());  
      expr_block->append(coerce_stmt);
    }
    op_block->append(expr_block);
  }
  return op_block;
}

Type* OpNode::typeCheck() {

  bool is_error = false;

  for(unsigned int i = 0; i<arity(); ++i){
    if(arg(i) == NULL){
      cout << "A" << endl;
      is_error = true;
      continue;
    }
    arg(i)->typeCheck();
    
    if(arg(i)->type()->tag() == Type::TypeTag::ERROR){
      is_error = true;
    }
  }

  if(is_error){
    type(new Type(Type::TypeTag::ERROR));
    return NULL;
  }



  int iopcode = static_cast<int>(opCode_);

  if(isArithmetic()){
    if(arity() == 1){
      Type* e_type = arg(0)->type();
      pair<bool,bool> check_sub_float  = Type::isSubType(e_type,Type::TypeTag::DOUBLE);

      //Ensures that argument is a subtype of DOUBLE
      if(!check_sub_float.first){
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      if((e_type->tag() == Type::TypeTag::BYTE) || (e_type->tag() == Type::TypeTag::UINT)){
        Type* new_type = new Type(Type::TypeTag::INT);
        arg(0)->coercedType(new_type);
        e_type = new_type;
        type(e_type);
        return e_type;
      }

      type(e_type);
      return e_type;
    }
    if(arity() == 2){
      Type* e1_type = arg(0)->type();
      Type* e2_type = arg(1)->type();
      pair<bool,bool> check_e1_sub_float  = Type::isSubType(e1_type,Type::TypeTag::DOUBLE);
      pair<bool,bool> check_e2_sub_float  = Type::isSubType(e2_type,Type::TypeTag::DOUBLE);

      pair<bool,bool> check_e1_sub_int  = Type::isSubType(e1_type,Type::TypeTag::INT);
      pair<bool,bool> check_e2_sub_int  = Type::isSubType(e2_type,Type::TypeTag::INT);

      if(OpNode::opCode() == OpCode::MOD){
        if(!check_e1_sub_int.first){
          errMsg("Incompatible type for argument 1 for operator '%'",this);
          type(new Type(Type::TypeTag::ERROR));
          return NULL;
        }
        if(!check_e2_sub_int.first){
          errMsg("Incompatible type for argument 2 for operator '%'",this);
          type(new Type(Type::TypeTag::ERROR));
          return NULL;
        }
        if(e1_type->tag() == Type::TypeTag::INT && e2_type->tag() == Type::TypeTag::INT){
          type(e1_type);
          return e1_type;
        }
        if(e1_type->tag() == Type::TypeTag::INT && e2_type->tag() != Type::TypeTag::INT){
          arg(1)->coercedType(e1_type);
          type(e1_type);
          return e1_type;
        }
        if(e1_type->tag() != Type::TypeTag::INT && e2_type->tag() == Type::TypeTag::INT){
          arg(0)->coercedType(e2_type);
          type(e2_type);
          return e2_type;
        }
        if(e1_type->tag() != Type::TypeTag::INT && e2_type->tag() != Type::TypeTag::INT){
          Type* new_type = new Type(Type::TypeTag::INT);
          arg(0)->coercedType(new_type);
          arg(1)->coercedType(new_type);
          type(e1_type);
          return e1_type;
        }
      }
      if(!check_e1_sub_float.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      if(!check_e2_sub_float.first){
        errMsg(string("Incompatible type for argument 2 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      pair<bool,bool> compare_expr = Type::isSubType(e1_type,e2_type);
      if(compare_expr.first && compare_expr.second){
        type(e1_type);
        return e1_type;
      }
      if(!compare_expr.first && compare_expr.second){
        arg(1)->coercedType(e1_type);
        type(e1_type);
        return e1_type;
      }
      if(compare_expr.first && !compare_expr.second){
        arg(0)->coercedType(e2_type);
        type(e2_type);
        return e2_type;
      }
      errMsg("Arithmetic operator between incompatible types",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
  }
  if(isBitWise()){
    if(arity() == 1){
      Type* e_type = arg(0)->type();
      pair<bool,bool> check_sub_int  = Type::isSubType(e_type,Type::TypeTag::INT);

      //Ensures that argument is a subtype of INT
      if(!check_sub_int.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      if((e_type->tag() == Type::TypeTag::INT)){
        Type* new_type = new Type(Type::TypeTag::UINT);
        arg(0)->coercedType(new_type);
        e_type = new_type;
        type(e_type);
        return e_type;
      }

      type(e_type);
      return e_type;
    }
    if(arity() == 2){
      Type* e1_type = arg(0)->type();
      Type* e2_type = arg(1)->type();
      pair<bool,bool> check_e1_sub_int  = Type::isSubType(e1_type,Type::TypeTag::INT);
      pair<bool,bool> check_e2_sub_int  = Type::isSubType(e2_type,Type::TypeTag::INT);

      if(!check_e1_sub_int.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      if(!check_e2_sub_int.first){
        errMsg(string("Incompatible type for argument 2 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      pair<bool,bool> compare_expr = Type::isSubType(e1_type,e2_type);
      if(compare_expr.first && compare_expr.second){
        type(e1_type);
        return e1_type;
      }
      if(!compare_expr.first && compare_expr.second){
        arg(0)->coercedType(e2_type);
        type(e2_type);
        return e2_type;
      }
      if(compare_expr.first && !compare_expr.second){
        arg(1)->coercedType(e1_type);
        type(e1_type);
        return e1_type;
      }
      errMsg("Arithmetic operator between incompatible types",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
  }
  if(isRelational()){
    if(arity() == 1){
      errMsg("Relational operator has not enough parameters",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
    if(arity() == 2){
      Type* e1_type = arg(0)->type();
      Type* e2_type = arg(1)->type();
      pair<bool,bool> check_e1_sub_float  = Type::isSubType(e1_type,Type::TypeTag::DOUBLE);
      pair<bool,bool> check_e2_sub_float  = Type::isSubType(e2_type,Type::TypeTag::DOUBLE);

      if(!check_e1_sub_float.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }
      if(!check_e2_sub_float.first){
        errMsg(string("Incompatible type for argument 2 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::ERROR));
        return NULL;
      }

      Type* bool_type = new Type(Type::TypeTag::BOOL);
      pair<bool,bool> compare_expr = Type::isSubType(e1_type,e2_type);
      if(compare_expr.first && compare_expr.second){
        type(bool_type);
        return type();
      }
      if(!compare_expr.first && compare_expr.second){
        arg(1)->coercedType(e1_type);
        type(bool_type);
        return type();
      }
      if(compare_expr.first && !compare_expr.second){
        arg(0)->coercedType(e2_type);
        type(bool_type);
        return type();
      }
      errMsg("Relational operator between incompatible types",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
  }
  if(isLogical()){
    if(arity() == 1){
      Type* e_type = arg(0)->type();
      pair<bool,bool> check_sub_bool  = Type::isSubType(e_type,Type::TypeTag::BOOL);

      if(!check_sub_bool.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::BOOL));
        return NULL;
      }

      type(e_type);
      return e_type;
    }
    if(arity() == 2){
      Type* e1_type = arg(0)->type();
      Type* e2_type = arg(1)->type();
      pair<bool,bool> check_e1_sub_bool  = Type::isSubType(e1_type,Type::TypeTag::BOOL);
      pair<bool,bool> check_e2_sub_bool  = Type::isSubType(e2_type,Type::TypeTag::BOOL);

      if(!check_e1_sub_bool.first){
        errMsg(string("Incompatible type for argument 1 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::BOOL));
        return NULL;
      }
      if(!check_e2_sub_bool.first){
        errMsg(string("Incompatible type for argument 2 for operator '") + string(opInfo[iopcode].name_) +string("'"),this);
        type(new Type(Type::TypeTag::BOOL));
        return NULL;
      }

      type(e1_type);
      return e1_type;
    }
  }
  if(isAssign()){
    if(arity() == 1){
      errMsg("Assign operator has not enough parameters",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
    if(arity() == 2){
      Type* lhs_type = arg(0)->type();
      Type* rhs_type = arg(1)->type();
      pair<bool,bool> check_subtype = Type::isSubType(lhs_type,rhs_type);

      if(check_subtype.first && check_subtype.second){
        type(lhs_type);
        return lhs_type;
      }
      if(!check_subtype.first && check_subtype.second){
        arg(1)->coercedType(lhs_type);
        type(lhs_type);
        return lhs_type;
      }
      errMsg("Assigned expression must be a subtype of target",this);
      type(new Type(Type::TypeTag::ERROR));
      return NULL;
    }
  }
  errMsg("Unknown operator",this);
  type(new Type(Type::TypeTag::ERROR));
  return NULL;
}

void ValueNode::print(ostream& os, int indent) const {
  value()->print(os,indent);
  if(reg().isValid()){
    os << "(val_register_ = " << reg() << ")";
  }
}

void ValueNode::typePrint(ostream& os, int indent) const {
  if(coercedType() != NULL){
    os << "(" << coercedType()->fullName() << ")";
  }

  os << type()->fullName();
}

void ValueNode::memAlloc(MemoryMgr &mm){
  //Not directly necessary, but will hopefully be removed in optimization anyway
  if(Register::sameRegisters(type(),coercedType())){
    reg(mm.getNextRegister(type()));
    mm.addRegister(reg());
  } else {
    tempReg(mm.getNextRegister(type()));
    mm.addRegister(tempReg());
    reg(mm.getNextRegister(coercedType()));
    mm.addRegister(reg());
    mm.freeRegister(tempReg());
  }
}

CodeBlock* ValueNode::codeGen() {
  ICode::ICodeType val_type;
  CodeBlock* val_block = new CodeBlock();
  if(startLabel()!=NULL){
    val_block->setStartLabel(startLabel());
  }
  if(type()->tag() == Type::TypeTag::BOOL){
    Label* jmp_label = NULL;
    if(value()->bval()){
      jmp_label = trueLabel();
    } else {
      jmp_label = falseLabel();
    }
    ICode jmp(ICode::ICodeType::JMP,jmp_label);
    val_block->append(jmp);
    return val_block;
  } else if(type()->tag() == Type::TypeTag::STRING){
    val_type = ICode::ICodeType::MOVS;
  } else if(Register::isFloatRegister(type()->tag())){
    val_type = ICode::ICodeType::MOVF;
  } else {
    val_type = ICode::ICodeType::MOVI;
  }

  ICode mov_stmt(val_type,value(),&destReg());
  val_block->append(mov_stmt);
  if(!Register::sameRegisters(type(),coercedType())){
    ICode::ICodeType mov_type = ICode::getMovType(tempReg(),reg());
    ICode coerce_stmt = ICode(mov_type,&tempReg(),&reg());  
    val_block->append(coerce_stmt);
  }
  return val_block;
}

InvocationNode::InvocationNode(const SymTabEntry *ste, vector<ExprNode*>* param, 
     int line, int column, string file) :
  ExprNode(ExprNode::ExprNodeType::INV_NODE,NULL,line,column,file) {
  params_ = param;
  ste_ = ste;
  call_label_ = NULL;
}

InvocationNode::InvocationNode(const InvocationNode& inv_node) :
  ExprNode(inv_node) {
  params_ = inv_node.params_;
  ste_ = inv_node.symTabEntry();
  call_label_ = inv_node.callLabel();
}

void InvocationNode::print(ostream& os,int indent) const {
  bool print_offset = params_->size()==offsets_.size();
  os << symTabEntry()->name() << "(";
  for(unsigned int i=0;i<params()->size();++i){
    if(i!=0){
      os << " , ";
    }
    if((*params())[i] != NULL){
      (*params())[i]->print(os,0);
      if(print_offset){
        os << "(offset = " << offsets_[i] << ")";
      }
    }
  }
  if(reg().isValid()){
    os << ", fun_register_ = " << reg();
  }
  os << ")";
}

void InvocationNode::typePrint(ostream& os,int indent) const {
  os << "(";
  if(coercedType() != NULL){
    os << coercedType()->fullName() << " , ";
  }
  os << "ret_type = " << type()->fullName() << ")" << symTabEntry()->name() << "(";
  for(unsigned int i=0;i<params()->size();++i){
    if(i!=0){
      os << " , ";
    }
    if((*params())[i] != NULL){
      (*params())[i]->typePrint(os,0);
    }
  }
  os << ")";
}

void InvocationNode::memAlloc(MemoryMgr &mm){
  mem_register_ = mm.getNextRegister(true);
  mm.addRegister(mem_register_);
  if(params_!=NULL){
    for(unsigned int i=0;i<params()->size();++i){
      if((*params())[i] != NULL){
        (*params())[i]->memAlloc(mm);
      }
    }
    for(unsigned int i=0;i<params()->size();++i){
      if((*params())[i] != NULL){
        mm.freeRegister((*params())[i]->reg());
      }
    }
  }
  label_register_ = mm.getNextRegister(true);
  mm.addRegister(label_register_);
  mm.freeRegister(label_register_);
  mm.freeRegister(mem_register_);

  used_regs_ = mm.getUsedRegisters();
  callLabel(new Label(Label::LabelType::FUNC_CALL));
  return_address_ = mm.getNextAddress();

  offsets_ = vector<int>();
  //Loops through varaible initilizations
  const SymTab* st = ste_->symTab();
  if(st!=NULL){
    for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
      if((*i)!=NULL && (*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
        VariableEntry* ve = (VariableEntry*) (*i);
        if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
          //Parameters
          offsets_.push_back(ve->offSet());
        } else {
          break;
        }
      }
    }
  }
  if(Register::sameRegisters(type(),coercedType())){
    reg(mm.getNextRegister(type()));
    mm.addRegister(reg());
  } else {
    tempReg(mm.getNextRegister(type()));
    mm.addRegister(tempReg());
    reg(mm.getNextRegister(coercedType()));
    mm.addRegister(reg());
    mm.freeRegister(tempReg());
  }

  //Probably should be checking this
  if(params_->size()!=offsets_.size()){
    errMsg("Mismatch function arguments",this);
  }
}

Type* InvocationNode::typeCheck(){
  Type* f_type = const_cast<Type*>(symTabEntry()->type());

  type(f_type->retType());
  vector<Type*>* arg_types = f_type->argTypes();

  if(params()->size() != arg_types->size()){
    errMsg(to_string(arg_types->size()) + string(" arguments expected for ") + symTabEntry()->name(),this);
    return NULL;
  }
  
  for(unsigned int i = 0; i < params()->size(); ++i){

    param(i)->typeCheck();

    Type* param_type = param(i)->type();
    Type* arg_type = (*arg_types)[i];
    pair<bool,bool> check_subtype = Type::isSubType(param_type,arg_type);

    if(check_subtype.first && check_subtype.second){
      continue;
    }
    if(check_subtype.first && !check_subtype.second){
      param(i)->coercedType(arg_type);
      continue;
    }
    errMsg(string("Type mismatch for argument ") + to_string(i+1)  + string(" to ") + symTabEntry()->name(),this);
  }


  
  return NULL;
}

CodeBlock* InvocationNode::codeGen(){
  CodeBlock* func_block = new CodeBlock();

  if(startLabel()!=NULL){
    func_block->setStartLabel(startLabel());
  }

  //Store all used registers
  for(unsigned int i = 0; i < used_regs_.size();++i){
    Register* r = &used_regs_[i];
    ICode::ICodeType store_type;
    if(r->isInt()){
      store_type = ICode::ICodeType::STI;
    }
    if(r->isFloat()){
      store_type = ICode::ICodeType::STF;
    }
    ICode store_reg(store_type,r,&MemoryMgr::stackPointerRegister());
    ICode incr_stack(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());

    func_block->append(store_reg);
    func_block->append(incr_stack);
  }

  //Compute and store params
  if(params_!=NULL){
    for(unsigned int i=0;i<params()->size();++i){
      if((*params())[i] != NULL){
        func_block->append((*params())[i]->codeGen());
        //Put value in correct spot for function
        const Register* r = &(*params())[i]->reg();
        ICode::ICodeType store_type;
        if(r->isInt()){
          store_type = ICode::ICodeType::STI;
        }
        if(r->isFloat()){
          store_type = ICode::ICodeType::STF;
        }
        ICode get_addr(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(offsets_[i],Type::TypeTag::UINT),&mem_register_);
        ICode store_reg(store_type,r,&mem_register_);

        func_block->append(get_addr);
        func_block->append(store_reg);
      }
    }
  }
  //Store correct stuff for function call
  //Store return label
  ICode get_rl_addr(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(((FunctionEntry*)ste_)->returnLabel(),Type::TypeTag::UINT),&mem_register_);
  ICode mov_label(ICode::ICodeType::MOVL,callLabel(),&label_register_);
  ICode store_label(ICode::ICodeType::STI,&label_register_,&mem_register_);

  func_block->append(get_rl_addr);
  func_block->append(mov_label);
  func_block->append(store_label);

  //control link
  ICode get_cl_addr(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(((FunctionEntry*)ste_)->controlLink(),Type::TypeTag::UINT),&mem_register_);
  ICode store_cl(ICode::ICodeType::STI,&MemoryMgr::basePointerRegister(),&mem_register_);

  func_block->append(get_cl_addr);
  func_block->append(store_cl);

  //Set bp and sp
  ICode set_bp(ICode::ICodeType::MOVI,&MemoryMgr::stackPointerRegister(),&MemoryMgr::basePointerRegister());
  ICode set_sp(ICode::ICodeType::MOVI,&mem_register_,&MemoryMgr::stackPointerRegister());
  ICode incr_stack(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());
  
  func_block->append(set_bp);
  func_block->append(set_sp);
  func_block->append(incr_stack);

  //jmp to function
  ICode jmp_func(ICode::ICodeType::JMP,((FunctionEntry*)ste_)->funcStart());

  func_block->append(jmp_func);
  func_block->setEndLabel(callLabel());

  //Load return value
  ICode::ICodeType load_rv_type = ICode::ICodeType::LDI;
  if(destReg().isFloat()){
    load_rv_type = ICode::ICodeType::LDF;
  }

  ICode get_rv_addr(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value(((FunctionEntry*)ste_)->returnOffset(),Type::TypeTag::UINT),&mem_register_);
  ICode load_rv(load_rv_type,&mem_register_,&destReg());

  func_block->append(get_rv_addr);
  func_block->append(load_rv);

  if(!Register::sameRegisters(type(),coercedType())){
    ICode::ICodeType mov_type = ICode::getMovType(tempReg(),reg());
    ICode coerce_stmt = ICode(mov_type,&tempReg(),&reg());  
    func_block->append(coerce_stmt);
  }

  //Reset sp and bp
  ICode reset_sp(ICode::ICodeType::MOVI,&MemoryMgr::basePointerRegister(),&MemoryMgr::stackPointerRegister());
  ICode get_cl_addr2(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(((FunctionEntry*)ste_)->controlLink(),Type::TypeTag::UINT),&mem_register_);
  ICode reset_bp(ICode::ICodeType::LDI,&mem_register_,&MemoryMgr::basePointerRegister());

  func_block->append(reset_sp);
  func_block->append(get_cl_addr2);
  func_block->append(reset_bp);

  //Load all used registers
  for(unsigned int i = 0; i < used_regs_.size();++i){
    Register* r = &used_regs_[i];
    ICode::ICodeType store_type;
    if(r->isInt()){
      store_type = ICode::ICodeType::LDI;
    }
    if(r->isFloat()){
      store_type = ICode::ICodeType::LDF;
    }
    ICode decr_stack(ICode::ICodeType::SUB,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());
    ICode load_reg(store_type,&MemoryMgr::stackPointerRegister(),r);

    func_block->append(decr_stack);
    func_block->append(load_reg);
  }

  return func_block;
}

PrimitivePatNode::PrimitivePatNode(EventEntry* ee, vector<VariableEntry*>* params, 
           ExprNode* c,
           int line, int column, string file) : 
  BasePatNode(BasePatNode::PatNodeKind::PRIMITIVE,line,column,file){
  ee_ = ee;
  params_ = params;
  cond_ = c;
}

bool PrimitivePatNode::hasSeqOps() const{
  return false;
}
bool PrimitivePatNode::hasNeg() const{
  return false;
}
bool PrimitivePatNode::hasAnyOrOther() const{
  return false;
}

void PrimitivePatNode::print(ostream& os, int indent) const{
  if(event()->name() == "any"){
    os << event()->name();
  }else{
    os << event()->name() << "(";
    for(unsigned int i = 0 ; i < params_->size() ; ++i){
      if(i > 0){
        os<<", ";
      }
      os << (*params_)[i]->type()->fullName() << " " << (*params_)[i]->name();
      os << "(offset = " << (*params_)[i]->offSet() << ")";
    }
    os << ")";
  }
  if(cond()!=NULL){
    os << " | ";
    cond()->print(os,0);
  }
}

void PrimitivePatNode::typePrint(ostream& os, int indent) const{
  if(event()->name() == "any"){
    os << event()->name();
  }else{
    os << event()->name() << "(";
    for(unsigned int i = 0 ; i < params_->size() ; ++i){
      if(i > 0){
        os<<", ";
      }
      os << (*params_)[i]->type()->fullName() << " " << (*params_)[i]->name();
    }
    os << ")";
  }
  if(cond()!=NULL){
    os << " | ";
    cond()->typePrint(os,0);
  }
}

void PrimitivePatNode::memAlloc(MemoryMgr &mm){
  for(unsigned int i = 0;i<params_->size();++i){
    (*params_)[i]->memAlloc(mm);
    addInReg(mm.getNextRegister((*params_)[i]->type()));
    mm.addRegister(inReg(i));
    addMemReg(mm.getNextRegister(true));
    mm.addRegister(memReg(i));
    mm.freeRegister(inReg(i));
    mm.freeRegister(memReg(i));
  }
  if(cond()!=NULL){
    cond()->memAlloc(mm);
  }
}

CodeBlock* PrimitivePatNode::codeGen() {
  CodeBlock* prim_block = new CodeBlock();
  for(unsigned int i = 0;i<params_->size();++i){
    ICode::ICodeType in_type = ICode::ICodeType::INI;
    ICode::ICodeType store_type = ICode::ICodeType::STI;
    if(inReg(i).isFloat()){
      in_type = ICode::ICodeType::INF;
      store_type = ICode::ICodeType::STF;
    }
    ICode read_in_val(in_type,&inReg(i));
    ICode get_addr(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value((*params_)[i]->offSet(),Type::TypeTag::UINT),&memReg(i));
    ICode store_val(store_type,&inReg(i),&memReg(i));
    ICode incr_stack(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());

    prim_block->append(read_in_val);
    prim_block->append(get_addr);
    prim_block->append(store_val);
    prim_block->append(incr_stack);
  }
  return prim_block;
}

Type* PrimitivePatNode::typeCheck(){

  if(cond()!=NULL){
    cond()->typeCheck();

    Type* e_type = cond()->type();
    pair<bool,bool> check_sub_bool  = Type::isSubType(e_type,Type::TypeTag::BOOL);

    if(!check_sub_bool.first){
      errMsg("Boolean argument expected in Primitive Pattern",this);
    }
  }
  return NULL;
}

PatNode::PatNode(int line, int column, string file) :
  BasePatNode(BasePatNode::PatNodeKind::UNDEFINED,line,column,file){
  pat1_ = NULL;
  pat2_ = NULL;
}

PatNode::PatNode(PatNodeKind pk, BasePatNode *p1, BasePatNode*p2, int line, int column, string file) :
  BasePatNode(pk,line,column,file) {
  pat1_ = p1;
  pat2_ = p2;
}

void PatNode::print(ostream& os, int indent) const {
  if(pat1() == NULL || (pat2()==NULL && (kind()==BasePatNode::PatNodeKind::OR || kind()==BasePatNode::PatNodeKind::SEQ))){
    os<<"(ERROR)";
    return;
  }
  os << "(";
  if(kind()==BasePatNode::PatNodeKind::PRIMITIVE){
    pat1()->print(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::EMPTY){

  } else if(kind()==BasePatNode::PatNodeKind::NEG){
    os << "!";
    pat1()->print(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::SEQ){
    pat1()->print(os,0);
    os << ":";
    pat2()->print(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::OR){
    pat1()->print(os,0);
    os << "\\/";
    pat2()->print(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::STAR){
    pat1()->print(os,0);
    os << "**";
  } else if(kind()==BasePatNode::PatNodeKind::UNDEFINED){
    
  }
  os << ")";
}

void PatNode::typePrint(ostream& os, int indent) const {
  if(pat1() == NULL || (pat2()==NULL && (kind()==BasePatNode::PatNodeKind::OR || kind()==BasePatNode::PatNodeKind::SEQ))){
    os<<"(ERROR)";
    return;
  }
  os << "(";
  if(kind()==BasePatNode::PatNodeKind::PRIMITIVE){
    pat1()->typePrint(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::EMPTY){

  } else if(kind()==BasePatNode::PatNodeKind::NEG){
    os << "!";
    pat1()->typePrint(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::SEQ){
    pat1()->typePrint(os,0);
    os << ":";
    pat2()->typePrint(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::OR){
    pat1()->typePrint(os,0);
    os << "\\/";
    pat2()->typePrint(os,0);
  } else if(kind()==BasePatNode::PatNodeKind::STAR){
    pat1()->typePrint(os,0);
    os << "**";
  } else if(kind()==BasePatNode::PatNodeKind::UNDEFINED){
    
  }
  os << ")";
}

bool PatNode::hasSeqOps() const{
  return false;
}
bool PatNode::hasNeg() const{
  return false;
}
bool PatNode::hasAnyOrOther() const{
  return false;
}

void PatNode::memAlloc(MemoryMgr &mm){
  //cout << "PatNode memAlloc" << endl;
  if(pat1()!=NULL){
    pat1()->memAlloc(mm);
  }
  if(pat2()!=NULL){
    pat2()->memAlloc(mm);
  }
}

CodeBlock* PatNode::codeGen() {
  if(pat1() != NULL && kind() == BasePatNode::PatNodeKind::PRIMITIVE){
    return pat1()->codeGen();
  }
  return NULL;
}

Type* PatNode::typeCheck() {
  if(pat1() != NULL ){
    pat1()->typeCheck();
  }
  if(pat2() != NULL ){
    pat2()->typeCheck();
  }
  return NULL;
}

IfNode::IfNode(ExprNode* cond, StmtNode* thenStmt, 
     StmtNode* elseStmt, int line, int column, string file) :
  StmtNode(StmtNode::StmtNodeKind::IF,line,column,file) {
  cond_ = cond;
  then_ = thenStmt;
  else_ = elseStmt;

  true_label_ = NULL;
  false_label_ = NULL;
  end_label_ = NULL;
}

void IfNode::print(ostream& os, int indent) const{
  os << "if (";
  cond_->print(os,0);
  os << ") ";
  if(then_==NULL){
    os << "{}" <<endl;
  } else {
    then_->print(os,indent);
    if(then_->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR || then_->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){
      os << ";" << endl;
    }
  }
  if(else_!=NULL){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    os <<"else ";
    else_->print(os,indent);
    if(else_->stmtNodeKind()!=StmtNode::StmtNodeKind::COMPOUND){
      os << ";" << endl;
    }
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    os << ";" << endl;
  }
}

void IfNode::typePrint(ostream& os, int indent) const{
  os << "if (";
  cond_->typePrint(os,0);
  os << ") ";
  if(then_==NULL){
    os << "{}" <<endl;
  } else {
    then_->typePrint(os,indent);
    if(then_->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR || then_->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){
      os << ";" << endl;
    }
  }
  if(else_!=NULL){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    os <<"else ";
    else_->typePrint(os,indent);
    if(else_->stmtNodeKind()!=StmtNode::StmtNodeKind::COMPOUND){
      os << ";" << endl;
    }
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    os << ";" << endl;
  }
}

void IfNode::memAlloc(MemoryMgr &mm){
  falseLabel(new Label(Label::LabelType::BOOL_FALSE));
  trueLabel(new Label(Label::LabelType::BOOL_TRUE));
  endLabel(new Label(Label::LabelType::IF_END));
  //End label points to the same as false label if there is no else statement

  if(cond()!=NULL){
    cond()->trueLabel(trueLabel());
    cond()->falseLabel(falseLabel());
    cond()->memAlloc(mm);
    mm.freeRegister(cond()->reg());
  }
  if(thenStmt()!=NULL){
    thenStmt()->memAlloc(mm);
  }
  if(elseStmt()!=NULL){
    elseStmt()->memAlloc(mm);
  }
}

CodeBlock* IfNode::codeGen() {
  //Combine parts correctly and set labels as needed and pointers

  CodeBlock* if_block = new CodeBlock();

  if(cond()!=NULL){
    if_block->append(cond()->codeGen());
  }
  if(thenStmt()!=NULL){
    CodeBlock* then_stmt = thenStmt()->codeGen();
    then_stmt->setStartLabel(trueLabel());

    ICode jmp_end(ICode::ICodeType::JMP,endLabel());
    then_stmt->append(jmp_end);

    if_block->append(then_stmt);
  }
  if(elseStmt()!=NULL){
    CodeBlock* else_stmt = elseStmt()->codeGen();
    else_stmt->setStartLabel(falseLabel());
    if_block->append(else_stmt);
  } else {
    if_block->setEndLabel(falseLabel());
  }
  if_block->setEndLabel(endLabel());
  return if_block;
}

Type* IfNode::typeCheck() {
  if(cond()!=NULL){
    cond()->typeCheck();
    Type* e_type = cond()->type();

    if(e_type->tag() == Type::TypeTag::ERROR){
      return NULL;
    }

    pair<bool,bool> check_sub_bool  = Type::isSubType(e_type,Type::TypeTag::BOOL);

    if(!check_sub_bool.first){
      errMsg("Boolean argument expected",this);
      return NULL;
    }
  }
  if(thenStmt()!=NULL){
    thenStmt()->typeCheck();
  }
  if(elseStmt()!=NULL){
    elseStmt()->typeCheck();
  }
  return NULL;
}

WhileNode::WhileNode(ExprNode* cond, StmtNode* body, 
     int line, int column, string file) :
  StmtNode(StmtNode::StmtNodeKind::WHILE,line,column,file) {
  cond_ = cond;
  body_ = body;
}

void WhileNode::print(ostream& os, int indent) const{
  os << "while (";
  cond_->print(os,0);
  os << ") ";
  if(body_==NULL){
    os << "{}" <<endl;
  } else {
    body_->print(os,indent);
    if(body_->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR || body_->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN || body_->stmtNodeKind() == StmtNode::StmtNodeKind::BREAK){
      os << ";" << endl;
    }
  }
}

void WhileNode::typePrint(ostream& os, int indent) const{
  os << "while (";
  cond_->typePrint(os,0);
  os << ") ";
  if(body_==NULL){
    os << "{}" <<endl;
  } else {
    body_->typePrint(os,indent);
    if(body_->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR || body_->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN || body_->stmtNodeKind() == StmtNode::StmtNodeKind::BREAK){
      os << ";" << endl;
    }
  }
}

void WhileNode::memAlloc(MemoryMgr &mm){
  parent(mm.whileParent());
  mm.whileParent(this);

  begin(new Label(Label::LabelType::WHILE_BEGIN));
  bodyLabel(new Label(Label::LabelType::WHILE_BODY));
  end(new Label(Label::LabelType::WHILE_END));

  if(cond()!=NULL){
    cond()->trueLabel(bodyLabel());
    cond()->falseLabel(end());
    cond()->memAlloc(mm);
    mm.freeRegister(cond()->reg());
  }
  if(body()!=NULL){
    body()->memAlloc(mm);
  }
}

CodeBlock* WhileNode::codeGen() {
  // Set labels, and pointers

  CodeBlock* while_block = new CodeBlock();
  while_block->setStartLabel(begin());

  if(cond()!=NULL){
    while_block->append(cond()->codeGen());
  }
  if(body()!=NULL){
    CodeBlock* body_block = new CodeBlock();

    ICode jmp_begin(ICode::ICodeType::JMP,begin());
    
    body_block->setStartLabel(bodyLabel());
    body_block->append(body()->codeGen());
    body_block->append(jmp_begin);

    while_block->append(body_block);
  }

  while_block->setEndLabel(end());

  return while_block;
}

Type* WhileNode::typeCheck() {
  if(cond()!=NULL){
    cond()->typeCheck();
    Type* e_type = cond()->type();

    if(e_type->tag() == Type::TypeTag::ERROR){
      return NULL;
    }

    pair<bool,bool> check_sub_bool  = Type::isSubType(e_type,Type::TypeTag::BOOL);

    if(!check_sub_bool.first){
      errMsg("Boolean argument expected",this);
      return NULL;
    }
  }
  if(body()!=NULL){
    body()->typeCheck();
  }
  return NULL;
}

BreakNode::BreakNode(ValueNode* bl, int line, int column, string file) :
  StmtNode(StmtNode::StmtNodeKind::BREAK,line,column,file) {
  break_levels_ = bl;
}

void BreakNode::print(ostream& os, int indent) const{
  os << "break";
  if(breakLevels() != NULL){
    os << " ";
    breakLevels()->print(os,0);
  }
}

void BreakNode::memAlloc(MemoryMgr &mm){
  WhileNode* wn = mm.whileParent();
  int x = 1;
  while(wn != NULL && x < break_levels_->value()->ival()){
    wn = wn->parent();
    x++;
  }
  jmpLabel(wn->end());
}

CodeBlock* BreakNode::codeGen() {
  CodeBlock* break_block = new CodeBlock();

  ICode break_jmp(ICode::ICodeType::JMP,jmpLabel());

  break_block->append(break_jmp);

  return break_block;
}

void ReturnStmtNode::memAlloc(MemoryMgr &mm){
  expr_->memAlloc(mm);
  mem_reg_ = mm.getNextRegister(true);
  mm.addRegister(mem_reg_);
  label_reg_ = mm.getNextRegister(true);
  mm.addRegister(label_reg_);
  mm.freeRegister(expr_->reg());
  mm.freeRegister(label_reg_);
  mm.freeRegister(mem_reg_);
}

CodeBlock* ReturnStmtNode::codeGen() {
  //Place value of expression in correct memory location and then jump to where funciton is called
  CodeBlock* ret_block = new CodeBlock();

  if(expr_ != NULL){
    ret_block->append(expr_->codeGen());

    //Store return value
    ICode::ICodeType store_rv_type = ICode::ICodeType::STI;
    if(expr_->reg().isFloat()){
      store_rv_type = ICode::ICodeType::STF;
    }
    ICode get_rv_addr(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value(fun_->returnOffset(),Type::TypeTag::UINT),&mem_reg_);
    ICode store_rv(store_rv_type,&expr_->reg(),&mem_reg_);

    ret_block->append(get_rv_addr);
    ret_block->append(store_rv);
  }
  //jump back
  ICode lab_addr(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value(fun_->returnLabel(),Type::TypeTag::UINT),&mem_reg_);
  ICode load_lab(ICode::ICodeType::LDI,&mem_reg_,&label_reg_);
  ICode ret_jmp(ICode::ICodeType::JMPI,&label_reg_);

  ret_block->append(lab_addr);
  ret_block->append(load_lab);
  ret_block->append(ret_jmp);
  return ret_block;
}

Type* ReturnStmtNode::typeCheck(){
  if(expr_ != NULL){
    Type* ret_type = fun_->type()->retType();
    if(ret_type->tag() == Type::TypeTag::VOID){
      errMsg("No return value expected for a void function",this);
      return NULL;
    }
    expr_->typeCheck();
    Type* expr_type = expr_->type();
    
    pair<bool,bool> check_subtype = Type::isSubType(ret_type,expr_type);

    if(check_subtype.first && check_subtype.second){
      return NULL;
    }
    if(!check_subtype.first && check_subtype.second){
      expr_->coercedType(ret_type);
      return NULL;
    }
    errMsg("Return value incompatible with current function's type",this);
    return NULL;
  }
  return NULL;
}

void CompoundStmtNode::print(ostream& os, int indent) const{

  os << "{";
  if(stmts()->size()>0){
    os<<endl;
  }
  printWithoutBraces(os,indent);
  os << "};"<<endl;
}

void CompoundStmtNode::typePrint(ostream& os, int indent) const{

  os << "{";
  if(stmts()->size()>0){
    os<<endl;
  }
  typePrintWithoutBraces(os,indent);
  os << "};"<<endl;
}

void CompoundStmtNode::printWithoutBraces(ostream& os, int indent) const{
  for(list<StmtNode*>::const_iterator i = stmts()->begin();i!=stmts()->end();i++){
    if((*i)!=NULL){
      for(int i=0;i<indent+1;++i){
        os << "\t";
      }
      (*i)->print(os,indent+1);
      if((*i)->stmtNodeKind() == StmtNode::StmtNodeKind::EXPR || (*i)->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN  || (*i)->stmtNodeKind()==StmtNode::StmtNodeKind::BREAK){
        os << ";";
      }
      if((*i)->stmtNodeKind() != StmtNode::StmtNodeKind::IF && (*i)->stmtNodeKind() != StmtNode::StmtNodeKind::WHILE){
        os << endl;
      }
    }
  }
  if(stmts()->size()>0){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
  }
}

void CompoundStmtNode::typePrintWithoutBraces(ostream& os, int indent) const{
  for(list<StmtNode*>::const_iterator i = stmts()->begin();i!=stmts()->end();i++){
    if((*i)!=NULL){
      for(int i=0;i<indent+1;++i){
        os << "\t";
      }
      (*i)->typePrint(os,indent+1);
      if((*i)->stmtNodeKind() == StmtNode::StmtNodeKind::EXPR || (*i)->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){
        os << ";";
      }
      if((*i)->stmtNodeKind() != StmtNode::StmtNodeKind::IF){
        os << endl;
      }
    }
  }
  if(stmts()->size()>0){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
  }
}

Type* CompoundStmtNode::typeCheck(){
  for(list<StmtNode*>::iterator i = stmts()->begin();i!=stmts()->end();i++){
    if((*i)!=NULL){
      (*i)->typeCheck();
    }
  }
  return NULL;
}

void CompoundStmtNode::memAlloc(MemoryMgr &mm){
  for(list<StmtNode*>::iterator i = stmts()->begin();i!=stmts()->end();i++){
    if((*i)!=NULL){
      (*i)->memAlloc(mm);
    }
  }
}

CodeBlock* CompoundStmtNode::codeGen() {
  //Combine all codeblocks
  CodeBlock* cmp_block = new CodeBlock();
  for(list<StmtNode*>::iterator i = stmts()->begin();i!=stmts()->end();i++){
    if((*i)!=NULL){
      cmp_block->append((*i)->codeGen());
    }
  }
  return cmp_block;
}

RuleNode::RuleNode(BlockEntry *re, BasePatNode* pat, StmtNode* reaction, 
     int line, int column, string file) : 
    AstNode(AstNode::NodeType::RULE_NODE,line,column,file) {
  rste_ = re;
  pat_ = pat;
  reaction_ = reaction;

  start_label_ = NULL;
  return_label_ = NULL;
}

void RuleNode::print(ostream& os, int indent) const{
  if(pat()!= NULL && reaction()!=NULL){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    pat()->print(os,0);
    os << "-->  ";
    reaction()->print(os,indent);
    if(reaction()->stmtNodeKind() == StmtNode::StmtNodeKind::COMPOUND){
      for(int i=0;i<indent;++i){
        os << "\t";
      }
    }
    os << ";;" << endl;
  }
}

void RuleNode::typePrint(ostream& os, int indent) const{
  if(pat()!= NULL && reaction()!=NULL){
    for(int i=0;i<indent;++i){
      os << "\t";
    }
    pat()->typePrint(os,0);
    os << "-->  ";
    reaction()->typePrint(os,indent);
    if(reaction()->stmtNodeKind() == StmtNode::StmtNodeKind::COMPOUND){
      for(int i=0;i<indent;++i){
        os << "\t";
      }
    }
    os << ";;" << endl;
  }
}

Type* RuleNode::typeCheck(){
  if(pat() != NULL && reaction() != NULL){
    pat()->typeCheck();
    reaction()->typeCheck();
  }
  return NULL;
}

void RuleNode::memAlloc(MemoryMgr &mm){
  start_label_ = new Label(Label::LabelType::RULE_START);
  return_label_ = new Label(Label::LabelType::PARSE_RETURN);

  control_link_ = mm.getNextAddress();
  control_reg_ = mm.getNextRegister(true);
  mm.addRegister(control_reg_);
  mm.freeRegister(control_reg_);

  pat()->memAlloc(mm);
  reaction()->memAlloc(mm);
}

CodeBlock* RuleNode::codeGen() {
  CodeBlock* rule_block = new CodeBlock();
  rule_block->setStartLabel(startLabel());

  ICode control_addr(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(control_link_,Type::TypeTag::UINT),&control_reg_);
  ICode store_bp(ICode::ICodeType::STI,&MemoryMgr::basePointerRegister(),&control_reg_);
  ICode set_bp(ICode::ICodeType::MOVI,&MemoryMgr::stackPointerRegister(),&MemoryMgr::basePointerRegister());
  ICode incr_stack(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());

  rule_block->append(control_addr);
  rule_block->append(store_bp);
  rule_block->append(set_bp);
  rule_block->append(incr_stack);

  if(pat() != NULL){
    rule_block->append(pat()->codeGen());
  }

  if(reaction() != NULL){
    rule_block->append(reaction()->codeGen());
  }

  ICode control_addr2(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value(control_link_,Type::TypeTag::UINT),&control_reg_);
  ICode set_stack(ICode::ICodeType::MOVI,&MemoryMgr::basePointerRegister(),&MemoryMgr::stackPointerRegister());
  ICode load_bp(ICode::ICodeType::LDI,&control_reg_,&MemoryMgr::basePointerRegister());

  // Jump back to parsing
  ICode ret_jmp(ICode::ICodeType::JMP,returnLabel());

  rule_block->append(control_addr2);
  rule_block->append(set_stack);
  rule_block->append(load_bp);
  rule_block->append(ret_jmp);

  return rule_block;
}

void ExprStmtNode::memAlloc(MemoryMgr &mm){
  expr_->memAlloc(mm);
  mm.freeRegister(expr_->reg());
}

CodeBlock* ExprStmtNode::codeGen() {
  if(expr_ != NULL){
    return expr_->codeGen();
  }
  return NULL;
}

Type* ExprStmtNode::typeCheck(){
  expr_->typeCheck();
  return NULL;
}
