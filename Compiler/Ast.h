#ifndef AST_H
#define AST_H

#include "all.h"
#include "Value.h"
#include "ProgramElem.h"
#include "SymTabEntry.h"
#include "MemoryMgr.h"
#include "CodeBlock.h"

class BlockEntry;
class EFSA;
class EventEntry;
class FunctionEntry;
class LabelEntry;
class OpNode;
//enum class OpCode;
class PatNode;
//enum class PatNode;
class PrimitivePatNode;
class RulezNode;
class SymTabEntry;
class VariableEntry;

/*****************************************************************************
   Here is the class hierarchy:
                                               ProgramElem
											       |
                                                AstNode
     +--------------------------------------------+----------------+
     |		         |                 |                           | 
 BasePatNode      ExprNode          RuleNode                    StmtNode
     |               |                                             |
     |               |                                             |
     |      +---------+----------+-----------+                     |
     |      |         |          |           |                     |
     |  RefExprNode  OpNode  ValueNode  InvocationNode             |
     |                                                             |
     |                                      +---------------+------+-----+
     |                                      |               |            |
     |                                 ExprStmtNode   CompoundStmtNode IfNode
     |
     |
   +-+------------------+
   |                    |
   |                    |
PrimitivePatNode    PatNode

******************************************************************************/
    
class AstNode: public ProgramElem {
 public:
  enum class NodeType  {
    PAT_NODE,
    EXPR_NODE, 
    REACTION_NODE, 
    STMT_NODE, 
    SEQUENCE_NODE, 
    RULE_NODE
  };

 public: 
  AstNode(NodeType nt, int line=0, int column=0, string file="");
  AstNode(const AstNode&); // copy constructor 
  virtual ~AstNode() {};
  //virtual AstNode* clone() = 0;

  NodeType nodeType() const { return nodeType_;}

  virtual void print(ostream& os, int indent=0) const=0;
  virtual void typePrint(ostream& os, int indent=0) const=0;

  virtual void renameRV(string prefix) {}; // new names start with given prefix
  virtual bool operator==(const AstNode&) const { return false; };
  virtual bool operator!=(const AstNode& a) const 
  { return !operator==(a); };

  virtual void memAlloc(MemoryMgr &mm) {}
  virtual Type* typeCheck() { return NULL; }
  
 private: 
  NodeType nodeType_;
  const AstNode* operator=(const AstNode& other); /* disable asg */
};

inline ostream& operator<<(ostream& os, const AstNode& an) {
  an.print(os);
  return os;
};

/****************************************************************/
class ExprNode: public AstNode {
 public:
  enum class ExprNodeType   {
    REF_EXPR_NODE, 
    OP_NODE, 
    VALUE_NODE, 
    INV_NODE
   };

 public:
  ExprNode(ExprNodeType et, const Value* val=0, int line=0, int column=0,
		   string file=""); // val is saved, but not copied
  ExprNode(const ExprNode&);
  virtual ~ExprNode() {};
 
  virtual ExprNode* clone() const=0;

  ExprNodeType exprNodeType() const { return exprType_;};
  void exprNodeType(ExprNodeType t) { exprType_ = t; };

  const Value* value() const { return val_; }

  //Coerced Type should be null if not coerced
  const Type* coercedType() const { return coercedType_; }
  void coercedType(const Type* type) { coercedType_ = type; }

  const Register& reg() const { return register_; }
  void reg(const Register& r) { register_ = r; }

  const Register& tempReg() const { return temp_reg_; }
  void tempReg(const Register& r) { temp_reg_ = r; }

  const Register& destReg() const {
    if(Register::sameRegisters(type(),coercedType())){
      return reg();
    } else {
      return tempReg();
    }
  }

  void print(ostream& os, int indent=0) const=0;

  virtual void memAlloc(MemoryMgr &mm) {}

  Label* startLabel() const { return start_label_; }
  void startLabel(Label* sl) { start_label_ = sl;}

  Label* endLabel() const { return end_label_; }
  void endLabel(Label* sl) { end_label_ = sl;}

  Label* trueLabel() const { return true_label_; }
  void trueLabel(Label* tl) { true_label_ = tl;}

  Label* falseLabel() const { return false_label_; }
  void falseLabel(Label* fl) { false_label_ = fl;}

 private:
  ExprNodeType exprType_;
  const Value *val_; // reference semantics for val_ and coercedType_
  const Type* coercedType_;

  //Holds number of register and if Float or Int register
  Register register_;
  Register temp_reg_;

  Label* start_label_;
  Label* end_label_;
  Label* true_label_;
  Label* false_label_;
};

/****************************************************************/
class RefExprNode: public ExprNode {
 public:
  RefExprNode(string ext, const SymTabEntry* ste=NULL, 
	      int line=0, int column=0, string file="");
  RefExprNode(const RefExprNode&);
  ExprNode* clone() const { return new RefExprNode(*this); }

  ~RefExprNode() {};

  string ext() const { return ext_;};
  void ext(string str) { ext_ = str;}; 

  const SymTabEntry* symTabEntry() const { return sym_;};
  void symTabEntry(const SymTabEntry *ste)  { sym_ = ste;};

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  void lhsMemAlloc(MemoryMgr &mm);

  void offSet(int of) { offset_ = of; }
  int offSet() const { return offset_; }

  void fromBP(bool bp) { from_bp_ = bp; }
  bool fromBP() const { return from_bp_; }

  void memReg(Register mr) { mem_register_ = mr; }
  const Register& memReg() const { return mem_register_; }

  CodeBlock* codeGen();
  CodeBlock* getAddrCodeGen();

 private:
  string ext_;
  const SymTabEntry* sym_;

  Register mem_register_;

  int offset_;
  bool from_bp_;
};

/****************************************************************/

#define MAX_OP_ARITY 2

class OpNode: public ExprNode {
 public:
  enum class OpCode {
    UMINUS, PLUS, MINUS, MULT, DIV, MOD, 
    EQ, NE, GT, LT, GE, LE,
    AND, OR, NOT, 
    BITNOT, BITAND, BITOR, BITXOR, SHL, SHR,
    ASSIGN, PRINT, INVALID
  };
    
  enum class OpPrintType {PREFIX, INFIX, POSTFIX};
  struct OpInfo {
    OpCode code_;
    const char* name_;
    int arity_;
		int needParen_;
    OpPrintType prtType_;
    Type::TypeTag argType_[3]; 
    // operators with > 2 args can be supported
    // only if types of args k through N are identical, for 1 <= k <= 3, 
    // and are given by argType[k-1]
    Type::TypeTag outType_;
		const char *typeConstraints_;
    ICode::ICodeType int_icodeType_;
    ICode::ICodeType float_icodeType_;
  };

 public:
  static const int VARIABLE = 100;
 public:
  OpNode(OpCode op, ExprNode *l, ExprNode *r=NULL,
	 int line=0, int column=0, string file="");
  OpNode(const OpNode&);
  ExprNode* clone() const { return new OpNode(*this); }
  ~OpNode() {};

  OpCode opCode() const { return opCode_;};
  const ExprNode* arg(unsigned int i) const 
    { return (i < arg_.size())? arg_[i] : NULL; };
  unsigned int arity() const { return arity_; }

  void opCode(OpCode a) { opCode_ = a; };
  ExprNode* arg(unsigned int i) 
    { return (i < arg_.size())? arg_[i] : NULL; };
  vector<ExprNode*>* args() 
    { return &arg_; }

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

  bool isArithmetic() const {
     return opCode() == OpCode::UMINUS || opCode() == OpCode::PLUS || opCode() == OpCode::MINUS || opCode() == OpCode::MULT || opCode() == OpCode::DIV || opCode() == OpCode::MOD;
  }
  bool isBitWise() const {
    return opCode() == OpCode::BITNOT || opCode() == OpCode::BITAND || opCode() == OpCode::BITOR || opCode() == OpCode::BITXOR || opCode() == OpCode::SHL || opCode() == OpCode::SHR;
  }
  bool isRelational() const{
    return opCode() == OpCode::EQ || opCode() == OpCode::NE || opCode() == OpCode::GT || opCode() == OpCode::LT || opCode() == OpCode::GE || opCode() == OpCode::LE;
  }
  bool isLogical() const{
    return opCode() == OpCode::AND || opCode() == OpCode::OR || opCode() == OpCode::NOT;
  }
  bool isAssign() const{
    return opCode() == OpCode::ASSIGN;
  }
  bool isBoolAssign() const{
    return isAssign() && type()!=NULL && type()->tag() == Type::TypeTag::BOOL;
  }
  
 private: 
  unsigned int arity_;
  OpCode   opCode_;
  vector<ExprNode*> arg_;
};

/****************************************************************/

class ValueNode: public ExprNode {
 public:
  ValueNode(Value* val=0, int line=0, int column=0, string file="")
    : ExprNode(ExprNode::ExprNodeType::VALUE_NODE, val, line,column,file) {
    if (val != NULL) type((Type*)(val->type()));
  }
  ValueNode(const ValueNode& val): ExprNode(val) {};
  ExprNode* clone() const { return new ValueNode(*this); }
  ~ValueNode() {};

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);

  CodeBlock* codeGen();

 private:
  /* val_ field is already included in ExprNode, so no new data members */
};

/****************************************************************/

class InvocationNode: public ExprNode {
  // Used to represent function invocation
 public:
  InvocationNode(const SymTabEntry *ste, vector<ExprNode*>* param=0, 
		 int line=0, int column=0, string file="");
  InvocationNode(const InvocationNode&);
  ExprNode* clone() const  { return new InvocationNode(*this); }
  ~InvocationNode() {};

  const SymTabEntry* symTabEntry() const { return ste_; };

  const vector<ExprNode*>* params() const { return params_;};
  vector<ExprNode*>* params() { return params_;}
  void params(vector<ExprNode*>* args){ params_ = args;};
  const ExprNode* param(unsigned int i) const 
    { return (params_ != NULL && i < params_->size())? (const ExprNode*)((*params_)[i]) : NULL; };
  ExprNode* param(unsigned int i)
    { return (params_ != NULL && i < params_->size())? ((*params_)[i]) : NULL;}
  void param(ExprNode* arg, unsigned int i) 
    { if (params_ != NULL && i < params_->size()) (*params_)[i] = arg;};

  Label* callLabel() const { return call_label_; }
  void callLabel(Label* cl) { call_label_ = cl; }

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  Type* typeCheck();

  CodeBlock* codeGen();

 private:
  vector<ExprNode*>* params_;
  const SymTabEntry *ste_; // reference semantics

  vector<Register> used_regs_;
  vector<int> offsets_;
  int return_address_;

  Register mem_register_;
  Register label_register_;

  Label* call_label_;
};

/****************************************************************/
// There are 3 kinds of PatNodes:
//   PrimitivePatNodes are of the form: event|cond
//   AtomicPatNodes consist of PrimitivePatNodes with one or more "||"
//      operators, plus an optional negation symbol
//   PatNodes consist of PatNodes or AtomicPatNodes composed with
//      ".", "||" and "*" operators
// We have a single base class for pattern nodes called BasePatNode. In
// addition, the functionality of Atomic and PatNodes have been
// combined into a single class PatNode.

class BasePatNode: public AstNode {
 public:
  enum class PatNodeKind {PRIMITIVE, EMPTY, NEG, SEQ, OR, STAR, UNDEFINED};

 public:
  BasePatNode(PatNodeKind pk, int ln=0, int col=0, string f=""):
	AstNode(AstNode::NodeType::PAT_NODE, ln, col, f) {
	parent_ = NULL; root_ = NULL; patKind_ = pk;};
  BasePatNode(const BasePatNode& bpn): AstNode(bpn) {
	patKind_ = bpn.patKind_; parent_ = bpn.parent_; root_ = bpn.root_;}
  ~BasePatNode() {};
  //virtual BasepatNode* clone() const { return new BasePatNode(*this);}	

  PatNodeKind kind() const { return patKind_; };
  void kind(PatNodeKind p) {patKind_ = p;}

  const BasePatNode* parent() const { return parent_; } 
  BasePatNode* parent() { return parent_;} 

  virtual bool hasSeqOps() const=0;
  virtual bool hasNeg() const=0;
  virtual bool hasAnyOrOther() const=0;
  virtual bool isNegatable() const {
	return ((!hasSeqOps()) && (!hasNeg())); }


 private:
  PatNodeKind patKind_;
  BasePatNode* parent_;
  BasePatNode* root_;
};

/****************************************************************/

class PrimitivePatNode: public BasePatNode {
 public:
  PrimitivePatNode(EventEntry* ee, vector<VariableEntry*>* params, 
				   ExprNode* c=NULL,
				   int line=0, int column=0, string file="");
  //PrimitivePatNode(const PrimitivePatNode& ppn);
  ~PrimitivePatNode() {};
  //BasePatNode* clone() { return new PrimitivePatNode(*this); }

  const EventEntry* event() const { return ee_; }
  EventEntry* event() { return ee_; }

  const vector<const VariableEntry*>* params() const { 
	return (vector<const VariableEntry*>*)params_; }
  vector<VariableEntry*>* params() { return params_; }

  const ExprNode* cond() const { return cond_; }
  ExprNode* cond() { return cond_; }
  void cond(ExprNode* c) { cond_ = c;}

  ExprNode* condition() { return condition_; }
  const ExprNode* condition() const { return condition_; }

  const list<const OpNode*>& asgs() const { 
	return (list<const OpNode*>&)asgs_; }  
  list<OpNode*>& asgs() { return asgs_; }  

  bool hasSeqOps() const;
  bool hasNeg() const;
  bool hasAnyOrOther() const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();
  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  Register& inReg(int i) { return in_regs_[i]; }
  void addInReg(Register r) { in_regs_.push_back(r); }

  Register& memReg(int i) { return mem_regs_[i]; }
  void addMemReg(Register r) { mem_regs_.push_back(r); }

 private:

  EventEntry* ee_;
  vector<VariableEntry*>* params_;
  /* cond_ may contain assignments as well as other expressions */
  /* condition_ contains all expresions in cond_ other than assignments */
  ExprNode* cond_;      
  ExprNode* condition_; 
  list<OpNode*> asgs_;
  
  vector<Register> in_regs_;
  vector<Register> mem_regs_;
};

/****************************************************************/
class PatNode: public BasePatNode {
 public: 
  PatNode(int line=0, int column=0, string file="");
  PatNode(PatNodeKind pk, BasePatNode *p1, BasePatNode*p2=NULL, int line=0, int column=0, string file="");
  
  ~PatNode() {};
  //AstNode* clone() 
  //  { return new PatNode(*this); }

  const BasePatNode* pat1() const { return pat1_; }
  BasePatNode* pat1() { return pat1_; }
  const BasePatNode* pat2() const { return pat2_; }
  BasePatNode* pat2() { return pat2_; }

  bool hasNeg() const;
  bool hasSeqOps() const;
  bool hasAnyOrOther() const;

  void print(ostream& os, int indent=0) const; 
  void typePrint(ostream& os, int indent=0) const; 

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private: 
  PatNode(const PatNode&);

  BasePatNode *pat1_;
  BasePatNode *pat2_;
};


/****************************************************************/

class StmtNode: public AstNode {
 public:
  enum class StmtNodeKind { ILLEGAL=-1, EXPR, IF, COMPOUND, RETURN , WHILE , BREAK };
 public: 
  StmtNode(StmtNodeKind skm, int line=0, int column=0, string file=""):
	AstNode(AstNode::NodeType::STMT_NODE, line,column,file) { skind_ = skm; };
  ~StmtNode() {};
  //AstNode* clone() 
  //  { return new StmtNode(*this); }

  StmtNodeKind stmtNodeKind() const { return skind_;}

  void print(ostream& os, int indent) const = 0;
  void typePrint(ostream& os, int indent) const = 0;

  void memAlloc(MemoryMgr &mm) { cout << "AAAAAA" << endl; }

 private:
  StmtNodeKind skind_;
};

/****************************************************************/

class ReturnStmtNode: public StmtNode {
 public:
  ReturnStmtNode(ExprNode *e, FunctionEntry* fe, 
				 int line=0, int column=0, string file=""):
    StmtNode(StmtNode::StmtNodeKind::RETURN,line,column,file) { expr_ = e; fun_ = fe;};
  ~ReturnStmtNode() {};

  void print(ostream& os, int indent) const {
	os << "return "; 
	if(expr_ != NULL) expr_->print(os, indent); else os << "NULL";};

  void typePrint(ostream& os, int indent) const {
  os << "return "; 
  if(expr_ != NULL) expr_->typePrint(os, indent); else os << "NULL";};


  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private:

  ExprNode* expr_;
  FunctionEntry* fun_;

  Register label_reg_;
  Register mem_reg_;
};

/****************************************************************/

class ExprStmtNode: public StmtNode {
 public:
  ExprStmtNode(ExprNode* e, int line=0, int column=0, string file=""):
    StmtNode(StmtNode::StmtNodeKind::EXPR,line,column,file) { expr_ = e; };
  ~ExprStmtNode() {};
  //AstNode* clone() 
  //  { return new ExprStmtNode(*this); }

  void print(ostream& os, int indent) const { 
	if (expr_ != NULL) { expr_->print(os, indent);}};
  void typePrint(ostream& os, int indent) const { 
  if (expr_ != NULL) { expr_->typePrint(os, indent);}};

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private:
  ExprNode* expr_;
};

/****************************************************************/

class CompoundStmtNode: public StmtNode{
 public: 
  CompoundStmtNode(list<StmtNode*> *Slist, int ln=0, int col=0, string fl=""):
	StmtNode(StmtNode::StmtNodeKind::COMPOUND, ln,col,fl) { stmts_ = Slist;};
  ~CompoundStmtNode() { };
  //AstNode* clone() 
  //  { return new CompoundStmtNode(*this); }

  const list<StmtNode*>* stmts() const { return stmts_;}

  list<StmtNode*>* stmts() { return stmts_;}
  void add(StmtNode *s) 
    { if(stmts_ != NULL) stmts_->push_back(s); };
  
  void  printWithoutBraces(ostream& os, int indent) const;
  void  print(ostream& os, int indent) const;

  void  typePrintWithoutBraces(ostream& os, int indent) const;
  void  typePrint(ostream& os, int indent) const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private:
  CompoundStmtNode(const CompoundStmtNode&);

  list<StmtNode*>   *stmts_;
};

/****************************************************************/

class IfNode: public StmtNode{
 public: 
  
  IfNode(ExprNode* cond, StmtNode* thenStmt, 
		 StmtNode* elseStmt=NULL, int line=0, int column=0, string file="");
  ~IfNode(){};
  //AstNode* clone() 
  //  { return new IfNode(*this); }

  const ExprNode* cond() const {return cond_;}
  const StmtNode* elseStmt() const { return else_;};
  const StmtNode* thenStmt() const  { return then_;};

  ExprNode* cond() {return cond_;}      
  StmtNode* elseStmt() { return else_;};
  StmtNode* thenStmt() { return then_;};

  Label* falseLabel() const { return false_label_; }
  Label* trueLabel() const { return true_label_; }
  Label* endLabel() const { return end_label_; }

  void falseLabel(Label* fl) { false_label_ = fl; }
  void trueLabel(Label* tl) { true_label_ = tl; }
  void endLabel(Label* el) { end_label_ = el; }

  void print(ostream& os, int indent) const;
  void typePrint(ostream& os, int indent) const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private: 
  ExprNode *cond_;
  StmtNode *then_, *else_;

  Label *true_label_, *false_label_, *end_label_;

  IfNode(const IfNode&);
};

/****************************************************************/

class WhileNode: public StmtNode{
 public:
  WhileNode(ExprNode* cond,StmtNode* body, int line=0, int column=0, string file="");
  ~WhileNode(){};

  ExprNode* cond() const { return cond_; }
  StmtNode* body() const { return body_; }

  Label* begin() const { return begin_; }
  Label* bodyLabel() const { return body_label_; }
  Label* end() const { return end_; }

  void begin(Label* b) { begin_ = b; }
  void bodyLabel(Label* b) { body_label_ = b; }
  void end(Label* e) { end_ = e; }

  WhileNode* parent() const { return parent_; }
  void parent(WhileNode* p) { parent_ = p; }

  void print(ostream& os, int indent) const;
  void typePrint(ostream& os, int indent) const;

  Type* typeCheck();

  void memAlloc(MemoryMgr &mm);

  CodeBlock* codeGen();

 private:
  ExprNode *cond_;
  StmtNode *body_;

  Label* begin_;
  Label* body_label_;
  Label* end_;

  WhileNode* parent_;

  WhileNode(const WhileNode&);
};

/****************************************************************/

class BreakNode: public StmtNode{
 public:
  BreakNode(ValueNode* bl, int line=0, int column=0, string file="");
  ~BreakNode(){};

  ValueNode* breakLevels() const { return break_levels_; }

  Label* jmpLabel() const { return jmp_label_; }
  void jmpLabel(Label* jl) { jmp_label_ = jl; }

  void print(ostream& os, int indent) const;
  void typePrint(ostream& os,int indent) const { print(os,indent); }

  void memAlloc(MemoryMgr &mm);

  CodeBlock* codeGen();

 private:
  ValueNode* break_levels_;

  Label* jmp_label_;

  BreakNode(const BreakNode&);
};

/****************************************************************/

class RuleNode: public AstNode {
 public:
  RuleNode(BlockEntry *re, BasePatNode* pat, StmtNode* reaction, 
	   int line=0, int column=0, string file="");
  ~RuleNode() {};
  //AstNode* clone() 
  //  { return new RuleNode(*this); }

  const BlockEntry* ruleEntry() const { return rste_; };
  BlockEntry* ruleEntry() { return rste_; };

  const BasePatNode* pat() const { return pat_; };
  BasePatNode* pat() { return pat_; };              

  const StmtNode* reaction() const { return reaction_; };   
  StmtNode* reaction() { return reaction_; };

  Label* startLabel() const { return start_label_; }
  void startLabel(Label* sl) { start_label_ = sl; }

  Label* returnLabel() const { return return_label_; }
  void returnLabel(Label* rl) { return_label_ = rl; }

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private:
  BlockEntry    *rste_;
  BasePatNode *pat_;
  StmtNode *reaction_;

  Label* start_label_;
  Label* return_label_;

  int control_link_;
  Register control_reg_;
   
  RuleNode(const RuleNode&);
};

/****************************************************************/
#endif
