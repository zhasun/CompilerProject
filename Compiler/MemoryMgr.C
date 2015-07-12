
#include "ParserUtil.h" //Only for itoa
#include "MemoryMgr.h"

int Label::var_counts_[NUM_LABEL_TYPE];
const string Label::var_names_[NUM_LABEL_TYPE] = {"none","while_begin","while_body","while_end","bool_false","bool_true","bool_start","bool_end","if_end","func_call","func_decl","func_end","rule_start","parse_start","parse_return","end_program"};

string Label::newName(LabelType lt){
	int i = (int)lt;
	return var_names_[i]+"__"+itoa(var_counts_[i]++);
}

ostream& operator<<(ostream& os,const Label& l){
	os << l.name() << ":";
	return os;
}

ostream& operator<<(ostream& os,const Register& r){
	string r_str = to_string(r.val());
	while(r_str.size() < 3){
		r_str = string("0") + r_str;
	}
	if(r.isInt()){
		os << "R" << r_str;
	}
	if(r.isFloat()){
		os << "F" << r_str;
	}
	return os;
}

bool operator==(const Label& l1,const Label& l2){
	return l1.name() == l2.name();
}

bool operator==(const Register& r1,const Register& r2){
	return r1.isInt() == r2.isInt() && r1.val() == r2.val();
}

const Register MemoryMgr::base_pointer_ = Register(NUM_REGISTERS-1,true);
const Register MemoryMgr::stack_pointer_ = Register(NUM_REGISTERS-2,true);

bool Register::isIntRegister(Type::TypeTag t){
	if(t == Type::TypeTag::INT || t == Type::TypeTag::UINT || t == Type::TypeTag::BYTE || t == Type::TypeTag::STRING || t == Type::TypeTag::BOOL){
		return true;
	}
	return false;
}

bool Register::sameRegisters(const Type* t1,const Type* t2) {
	if(t1 == NULL || t2 == NULL){
		return true;
	}
	return  isIntRegister(t1->tag()) == isIntRegister(t2->tag());
}

bool MemoryMgr::addRegister(Register r) {
	bool* table = getTable(r.isInt());
	if(r.val() < 0 || r.val() >= NUM_REGISTERS || table[r.val()]){
		return false;
	}
	table[r.val()] = true;
	return true;
}

bool MemoryMgr::freeRegister(Register r) {
	bool* table = getTable(r.isInt());
	if(r.val() <0 || r.val() >= NUM_REGISTERS || !table[r.val()]){
		return false;
	}
	table[r.val()] = false;
	return true;
}

Register MemoryMgr::getNextRegister(const Type* t){
	if(t == NULL){
		cout << "NULL TYPE" << endl;
		return Register();
	}
	bool isint = Register::isIntRegister(t->tag());
	//Check if Float, and if neither then error
	return getNextRegister(isint);
}

Register MemoryMgr::getNextRegister(bool is_int){
	bool* table = getTable(is_int);
	int x = 0;
	if(is_int){
		x = next_int_register_;
	} else {
		x = next_float_register_;
	}
	for(int i = 0; i < NUM_REGISTERS; ++i){
		if(!table[(i+x) % NUM_REGISTERS]){
			if(is_int){
				next_int_register_ = (i+x) % NUM_REGISTERS + 1;
			} else {
				next_float_register_ = (i+x) % NUM_REGISTERS + 1;
			}
			return Register((i+x) % NUM_REGISTERS,is_int);
		}
	}
	return Register();
}

vector<Register> MemoryMgr::getUsedRegisters(){
	vector<Register> used_regs;
	for(unsigned int i = 0; i < NUM_REGISTERS; ++i){
		//Excludes stack and base pointers
		if(int_registers_[i] && i != NUM_REGISTERS- 2 && i != NUM_REGISTERS - 1){
			used_regs.push_back(Register(i,true));
		}
		if(float_registers_[i]){
			used_regs.push_back(Register(i,false));
		}
	}
	return used_regs;
}

int MemoryMgr::getNextAddress() {
	int na = stack_pointer_value_;
	stack_pointer_value_++;
	return na;
}

