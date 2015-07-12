#include "STEClasses.h"
#include "Value.h"
#include "ParserUtil.h"
#include "MemoryMgr.h"

void GlobalEntry::print(ostream& out, int indent) const {

	out << endl;

	//Loops over all declarations
	const SymTab* st = this->symTab();
	for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
		(*i)->print(out,indent+1);
	}

	out << endl;

	//Loops over all Rules
	for(unsigned int x = 0; x < rules_.size();++x){
		rules_[x]->print(out,indent+1);
	} 

	out << endl;
}

void GlobalEntry::typePrint(ostream& out, int indent) const {

	out << endl;

	//Loops over all declarations
	const SymTab* st = this->symTab();
	for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
		(*i)->typePrint(out,indent+1);
	}

	out << endl;

	//Loops over all Rules
	for(unsigned int x = 0; x < rules_.size();++x){
		rules_[x]->typePrint(out,indent+1);
	} 

	out << endl;
}

Type* GlobalEntry::typeCheck(){
	SymTab* st = this->symTab();
	for(SymTab::iterator i = st->begin(); i != st->end(); ++i){
		(*i)->typeCheck();
	}

	//Loops over all Rules
	for(unsigned int x = 0; x < rules_.size();++x){
		rules_[x]->typeCheck();
	}
	return NULL;
}

void GlobalEntry::memAlloc(MemoryMgr &mm){
	memory_mgr_ = mm;

	parse_label_ = new Label(Label::LabelType::PARSE_START);
	end_label_ = new Label(Label::LabelType::END_PROGRAM);

	//Loops over all declarations
	SymTab* st = this->symTab();
	for(SymTab::iterator i = st->begin(); i != st->end(); ++i){
		(*i)->memAlloc(mm);
	}

	//Technically this happens between the two of them, but all registers should be free at this point
	in_reg_ = mm.getNextRegister(true);
	mm.addRegister(in_reg_);

	comp_reg_ = mm.getNextRegister(true);
	mm.addRegister(comp_reg_);


	
	mm.freeRegister(comp_reg_);

	//Loops over all Rules
	for(unsigned int x = 0; x < rules_.size();++x){
		MemoryMgr mm_r;
		mm_r.addRegister(in_reg_);
		rules_[x]->memAlloc(mm_r);
		mm_r.freeRegister(in_reg_);

		if(rules_[x]->pat()->kind() == BasePatNode::PatNodeKind::PRIMITIVE){
			PrimitivePatNode* pn = (PrimitivePatNode*)((PatNode*)rules_[x]->pat())->pat1();
			EventEntry* ee = pn->event();
			if(ee->name().length() == 1){
				rule_names_.push_back(ee->name());
				rule_labels_.push_back(rules_[x]->startLabel());
				rule_return_labels_.push_back(rules_[x]->returnLabel());
			}
			if(ee->name() == "any"){
				any_labels_.push_back(rules_[x]->startLabel());
				any_return_labels_.push_back(rules_[x]->returnLabel());
			}
		}
	}

	mm.freeRegister(in_reg_);
}

CodeBlock* GlobalEntry::codeGen(){
	CodeBlock* main_block = new CodeBlock();

	//--------------Init stack and base pointers--------------
	ICode set_stack(ICode::ICodeType::MOVI,new Value(0,Type::TypeTag::UINT),&MemoryMgr::stackPointerRegister());
	ICode set_base(ICode::ICodeType::MOVI,new Value(0,Type::TypeTag::UINT),&MemoryMgr::basePointerRegister());

	main_block->append(set_stack);
	main_block->append(set_base);

	//----------------------Declarations----------------------
	//Loops over all declarations
	SymTab* st = this->symTab();

	//Separates FunctionEntries from the other types
	vector<CodeBlock*> function_blocks;
	vector<CodeBlock*> other_blocks;
	for(SymTab::iterator i = st->begin(); i != st->end(); ++i){
		CodeBlock* cb = (*i)->codeGen();
		if((*i)->kind() == SymTabEntry::Kind::FUNCTION_KIND){
			function_blocks.push_back(cb);
		} else {
			other_blocks.push_back(cb);
		}
	}
	for(unsigned int x = 0; x < other_blocks.size();++x){
		main_block->append(other_blocks[x]);
	}
	ICode jmp_start(ICode::ICodeType::JMP,parse_label_);
	main_block->append(jmp_start);
	for(unsigned int x = 0; x < function_blocks.size();++x){
		main_block->append(function_blocks[x]);
	}

	//---------------------Event Matching---------------------

	// Reads in character
	CodeBlock* input_block = new CodeBlock();
	ICode read_in(ICode::ICodeType::IN,&in_reg_);
	
	//If negative, then reached end of file
	ICode* check_neg = new ICode(ICode::ICodeType::GT,new Value(0,Type::TypeTag::INT),&in_reg_);
	ICode end_jmp(ICode::ICodeType::JMPC,check_neg,end_label_);

	input_block->append(read_in);
	input_block->append(end_jmp);

	input_block->setStartLabel(parse_label_);

	CodeBlock* rule_block;
	// Named rules
	for(unsigned int i = 0; i < rule_names_.size(); ++i){
		rule_block = new CodeBlock();
		// Moves the string constant to the register
		ICode mov_string(ICode::ICodeType::MOVI,new Value((int)(rule_names_[i].at(0)),Type::TypeTag::INT),&comp_reg_);
		// Comparison
		ICode* compare = new ICode(ICode::ICodeType::EQ,&comp_reg_,&in_reg_);
		//Conditionally jumps
		ICode jmpc(ICode::ICodeType::JMPC,compare,rule_labels_[i]);

		rule_block->append(mov_string);
		rule_block->append(jmpc);
		rule_block->setEndLabel(rule_return_labels_[i]);

		input_block->append(rule_block);
	}

	// "Any" rules
	for(unsigned int i = 0; i < any_labels_.size(); ++i){
		rule_block = new CodeBlock();

		//Jumps to any rule body
		ICode jmp(ICode::ICodeType::JMP,any_labels_[i]);

		rule_block->append(jmp);
		rule_block->setEndLabel(any_return_labels_[i]);

		input_block->append(rule_block);
	}

	input_block->append(jmp_start);

	main_block->append(input_block);

	//------------------------Rules------------------------

	//Loops over all Rules
	for(unsigned int x = 0; x < rules_.size();++x){
		main_block->append(rules_[x]->codeGen());
	}

	//---------------------End program---------------------

	//Do the end jump here
	CodeBlock* end_block = new CodeBlock();
	ICode print_end(ICode::ICodeType::PRTS,new Value("PROGRAM END\\n"));
	end_block->setStartLabel(end_label_);
	end_block->append(print_end);

	main_block->append(end_block);

	return main_block;
}

void EventEntry::print(ostream& out, int indent) const{
	if(name()!="any"){
		for(int i=0;i<indent;++i){
			out << "\t";
		}
		out << "event "<< name() << "(";
		const SymTab* st = this->symTab();
		if(st!=NULL){
			for(SymTab::const_iterator i = st->begin(); i != st->end();){
				if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
					VariableEntry* ve = (VariableEntry*) (*i);
					if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
						out << ve->type()->fullName()<<" "<<ve->name();
						out << "(offset = " << ve->offSet() << ")";
					}
				}
				++i;
				if(i != st->end()){
					out << ", ";
				}
			}
		}
		out << ");"<<endl;
	}
}

void EventEntry::typePrint(ostream& out, int indent) const{
	if(name()!="any"){
		for(int i=0;i<indent;++i){
			out << "\t";
		}
		out << "event "<< name() << "(";
		const SymTab* st = this->symTab();
		if(st!=NULL){
			for(SymTab::const_iterator i = st->begin(); i != st->end();){
				if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
					VariableEntry* ve = (VariableEntry*) (*i);
					if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
						out << ve->type()->fullName()<<" "<<ve->name();
					}
				}
				++i;
				if(i != st->end()){
					out << ", ";
				}
			}
		}
		out << ");"<<endl;
	}
}

void EventEntry::memAlloc(MemoryMgr &mm){
	memory_mgr_ = MemoryMgr();
	//cout << "Event memAlloc" << endl;
	SymTab* st = this->symTab();
	if(st!=NULL){
		for(SymTab::iterator i = st->begin(); i != st->end();++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
					ve->offSet(memory_mgr_.getNextAddress());
				}
			}
		}
	}
}

Type* EventEntry::typeCheck(){
	vector<Type*>* arg_types = new vector<Type*>();

	SymTab* st = this->symTab();
	if(st!=NULL){
		for(SymTab::iterator i = st->begin(); i != st->end();++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
					arg_types->push_back(ve->type());
				}
			}
		}
	}
	type()->argTypes(arg_types);
	return NULL;
}

void ClassEntry::print(ostream& out, int indent) const{
	for(int i=0;i<indent;++i){
		out << "\t";
	}

	out << "class " << name() << ";" << endl;
}

void ClassEntry::typePrint(ostream& out, int indent) const{
	for(int i=0;i<indent;++i){
		out << "\t";
	}

	out << "class " << name() << ";" << endl;
}

void ClassEntry::memAlloc(MemoryMgr &mm){
	/*cout << "Class memAlloc" << endl;*/
}

void VariableEntry::print(ostream& out, int indent) const{
	if(type()==NULL || (type()->tag()==Type::TypeTag::CLASS && type()->typeDesc()==NULL)){
		return;
	}
	for(int i=0;i<indent;++i){
		out << "\t";
	}
	out << type()->fullName()<<" "<< name();
	out << "(offset = " << offSet();
	if(reg().isValid()){
		out << ", mem_register = " << reg();
	}
	out << ")";
	if(initVal()!=NULL){
		out << " = ";
		initVal()->print(out,0);
	}
	out << ";" << endl;
}

void VariableEntry::typePrint(ostream& out, int indent) const{
	if(type()==NULL || (type()->tag()==Type::TypeTag::CLASS && type()->typeDesc()==NULL)){
		return;
	}
	for(int i=0;i<indent;++i){
		out << "\t";
	}
	out << type()->fullName()<<" "<< name();
	if(initVal()!=NULL){
		out << " = ";
		initVal()->typePrint(out,0);
	}
	out << ";" << endl;
}

Type* VariableEntry::typeCheck(){
	if(initVal()!=NULL){
		initVal()->typeCheck();

		//Same as from OpNode()::typeCheck() isAssign() case
		Type* lhs_type = type();
		Type* rhs_type = initVal()->type();
		pair<bool,bool> check_subtype = Type::isSubType(lhs_type,rhs_type);

		if(check_subtype.first && check_subtype.second){
			return NULL;
		}
		if(!check_subtype.first && check_subtype.second){
			initVal()->coercedType(lhs_type);
			return NULL;
		}
		errMsg("Assignment between incompatible types",this);
	}
	return NULL;
}

void VariableEntry::memAlloc(MemoryMgr &mm){
	offSet(mm.getNextAddress());
	if(initVal()!=NULL){
		if(type()->tag() == Type::TypeTag::BOOL){
	      trueLabel(new Label(Label::LabelType::BOOL_TRUE));
	      falseLabel(new Label(Label::LabelType::BOOL_FALSE));
	      endLabel(new Label(Label::LabelType::BOOL_END));
	      initVal()->trueLabel(trueLabel());
	      initVal()->falseLabel(falseLabel());
	    }
		reg(mm.getNextRegister(true));
		mm.addRegister(reg());
		initVal()->memAlloc(mm);
		if(type()->tag() != Type::TypeTag::BOOL){
			mm.freeRegister(initVal()->reg());
		}
		mm.freeRegister(reg());
	}
}

CodeBlock* VariableEntry::codeGen(){
	CodeBlock* var_block = new CodeBlock();
	if(initVal() != NULL){
		var_block->append(initVal()->codeGen());

		ICode::ICodeType store_type = ICode::ICodeType::STI;
		ICode get_addr;

		if(varKind() == VariableEntry::VarKind::GLOBAL_VAR){
			ICode::ICodeType mov_type = ICode::ICodeType::MOVI;
			get_addr = ICode(mov_type,new Value(offSet(),Type::TypeTag::INT),&mem_register_);
		} else {
			ICode::ICodeType add_type = ICode::ICodeType::ADD;
			get_addr = ICode(add_type,&MemoryMgr::basePointerRegister(),new Value(offSet(),Type::TypeTag::INT),&mem_register_);
		}

		if(type()->tag() == Type::TypeTag::BOOL){
			CodeBlock* true_block = new CodeBlock();

			ICode store_true_val(store_type,new Value(1,Type::TypeTag::UINT),&mem_register_);
			ICode jmp_end(ICode::ICodeType::JMP,endLabel());

			true_block->setStartLabel(trueLabel());
			true_block->append(get_addr);
			true_block->append(store_true_val);
			true_block->append(jmp_end);

			CodeBlock* false_block = new CodeBlock();

			ICode store_false_val(store_type,new Value(0,Type::TypeTag::UINT),&mem_register_);

			false_block->setStartLabel(falseLabel());
			false_block->append(get_addr);
			false_block->append(store_false_val);
			false_block->setEndLabel(endLabel());

			var_block->append(true_block);
			var_block->append(false_block);
		} else {
			if(initVal()->reg().isFloat()){
				store_type = ICode::ICodeType::STF;
			}
			ICode store_val(store_type,&initVal()->reg(),&mem_register_);
			

			var_block->append(get_addr);
			var_block->append(store_val);
			
		}
	}
	ICode incr_stack(ICode::ICodeType::ADD,&MemoryMgr::stackPointerRegister(),new Value(1,Type::TypeTag::INT),&MemoryMgr::stackPointerRegister());
	var_block->append(incr_stack);

	return var_block;
}

void FunctionEntry::print(ostream& out, int indent) const{
	for(int i=0;i<indent;++i){
		out << "\t";
	}
	out << type()->retType()->name() << " " << name() << "(";
	const SymTab* st = this->symTab();
	bool p_var = true;
	if(st!=NULL){
		for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR && p_var){
					if(i != st->begin()){
						out << ", ";
					}
					out << ve->type()->fullName()<<" "<<ve->name();
					out << "(offset = " << ve->offSet() << ")";
				} else {
					if(p_var){
						out << ")(return_label = " << returnLabel() << ", return_address = " << returnOffset() << ", control_link = " << controlLink() << ") {" << endl;
						p_var = false;
					}
					ve->print(out,indent+1);
				}
			}
		}
	}
	if(p_var){
		out << ")(return_address = " << returnOffset() << ", control_link = " << controlLink() << ")";
	}
	if(body()!=NULL){
		if(p_var){
			out<<" {" << endl;
		}
		body()->printWithoutBraces(out,indent);
		for(int i=0;i<indent-1;++i){
			out << "\t";
		}
		out << "}";
	}
	out<< ";" << endl;
}

void FunctionEntry::typePrint(ostream& out, int indent) const{
	for(int i=0;i<indent;++i){
		out << "\t";
	}
	out << type()->retType()->name() << " " << name() << "(";
	const SymTab* st = this->symTab();
	bool p_var = true;
	if(st!=NULL){
		for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR && p_var){
					if(i != st->begin()){
						out << ", ";
					}
					out << ve->type()->fullName()<<" "<<ve->name();
				} else {
					if(p_var){
						out << ") {" << endl;
						p_var = false;
					}
					ve->print(out,indent+1);
				}
			}
		}
	}
	if(p_var){
		out << ")";
	}
	if(body()!=NULL){
		if(p_var){
			out<<" {" << endl;
		}
		body()->typePrintWithoutBraces(out,indent);
		for(int i=0;i<indent-1;++i){
			out << "\t";
		}
		out << "}";
	}
	out<< ";" << endl;
}

Type* FunctionEntry::typeCheck(){
	vector<Type*>* arg_types = new vector<Type*>();

	const SymTab* st = this->symTab();
	if(st!=NULL){
		for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR){
					arg_types->push_back(ve->type());
				}
			}
		}
	}

	type()->argTypes(arg_types);

	if(body()!=NULL){
		body()->typeCheck();
	}
	return NULL;
}

void FunctionEntry::memAlloc(MemoryMgr &mm){
	memory_mgr_ = MemoryMgr();

	const SymTab* st = this->symTab();
	bool p_var = true;
	if(st!=NULL){
		for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
			if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
				VariableEntry* ve = (VariableEntry*) (*i);
				if(ve->varKind()==VariableEntry::VarKind::PARAM_VAR && p_var){
					//Parameters
					ve->offSet(memory_mgr_.getNextAddress());
				} else {
					if(p_var){
						//Set control_link_ and return_address_
						returnLabel(memory_mgr_.getNextAddress());
						returnOffset(memory_mgr_.getNextAddress());
						controlLink(memory_mgr_.getNextAddress());
						p_var = false;
					}
					//Local variables declared in function
					ve->memAlloc(memory_mgr_);
				}
			}
		}
		if(p_var){
			returnLabel(memory_mgr_.getNextAddress());
			returnOffset(memory_mgr_.getNextAddress());
			controlLink(memory_mgr_.getNextAddress());
		}
	}
	if(body()!=NULL){
		body()->memAlloc(memory_mgr_);

		//Used for returning in case function reaches the end of its body without a return statement
		mem_reg_ = memory_mgr_.getNextRegister(true);
		memory_mgr_.addRegister(mem_reg_);
		label_reg_ = memory_mgr_.getNextRegister(true);
		memory_mgr_.addRegister(label_reg_);
		memory_mgr_.freeRegister(mem_reg_);
		memory_mgr_.freeRegister(label_reg_);
	}
}

CodeBlock* FunctionEntry::codeGen() {
	if(body()!=NULL){

		CodeBlock* func_block = new CodeBlock();
		func_block->setStartLabel(func_start_);
		func_block->append(body()->codeGen());

		const SymTab* st = this->symTab();
		if(st!=NULL){
			for(SymTab::const_iterator i = st->begin(); i != st->end(); ++i){
				if((*i)->kind()==SymTabEntry::Kind::VARIABLE_KIND){
					VariableEntry* ve = (VariableEntry*) (*i);
					if(ve->varKind()!=VariableEntry::VarKind::PARAM_VAR){
						func_block->append(ve->codeGen());
					}
				}
			}
		}

		CodeBlock* func_end = new CodeBlock();

		//Loads label from memory
		ICode lab_addr(ICode::ICodeType::ADD,&MemoryMgr::basePointerRegister(),new Value(returnLabel(),Type::TypeTag::UINT),&mem_reg_);
		ICode load_lab(ICode::ICodeType::LDI,&mem_reg_,&label_reg_);
		ICode ret_jmp(ICode::ICodeType::JMPI,&label_reg_);

		func_end->append(lab_addr);
		func_end->append(load_lab);
		func_end->append(ret_jmp);

		func_block->append(func_end);

		return func_block;
	}
	return NULL;
}

void BlockEntry::print(ostream& out, int indent) const{
	out << endl;
}

void BlockEntry::memAlloc(MemoryMgr &mm){
	cout << "Block memAlloc" << endl;
}
