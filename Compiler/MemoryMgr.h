#ifndef _MEMORY_MGR_H_
#define _MEMORY_MGR_H_

#include <iostream>
#include "Type.h"
#include "ICodeValue.h"
using namespace std;

#define NUM_REGISTERS 1000
#define NUM_LABEL_TYPE 16

class WhileNode;

class Label: public ICodeValue {
  public:
  	enum class LabelType { //Not all types, just some to start
      //Don't change order
      //If add anymore please update NUM_LABEL_TYPE above and add correctname to var_names_ in MemoryMgr.C
  		NONE,
  		WHILE_BEGIN,
      WHILE_BODY,
  		WHILE_END,
  		BOOL_FALSE,
      BOOL_TRUE,
      BOOL_START,
      BOOL_END,
      IF_END,
      FUNC_CALL,
      FUNC_DECL,
      FUNC_END,
      RULE_START,
      PARSE_START,
      PARSE_RETURN,
      END_PROGRAM
  	};

  	Label() : ICodeValue(ICodeValue::IType::LABEL) {
  		label_type_ = LabelType::NONE;
  		name_ = Label::newName(label_type_);
  	}
  	Label(LabelType lt) : ICodeValue(ICodeValue::IType::LABEL) {
  		label_type_ = lt;
  		name_ = Label::newName(label_type_);
  	}
    Label(LabelType lt,string& name) : ICodeValue(ICodeValue::IType::LABEL) {
      label_type_ = lt;
      name_ = name;  // For things like functions which have names
    }

    const string& name() const { return name_; }
    void name(string n) { name_ = n; }

    const LabelType& labelType() const { return label_type_; }
    void labelType(LabelType& lt) { label_type_ = lt; }

    void regPrint(ostream& os) const { os << name(); }

    friend ostream& operator<<(ostream& os,const Label& l);
    friend bool operator==(const Label& l1,const Label& l2);

  private:
    static string newName(LabelType lt);
    static int var_counts_[NUM_LABEL_TYPE];
    static const string var_names_[NUM_LABEL_TYPE];

  	LabelType label_type_;
  	string name_;
};

class Register: public ICodeValue {
  public:
  	Register() : ICodeValue(ICodeValue::IType::REGISTER) {
  		register_ = -1;
  		is_int_ = false;
  	}

  	Register(int r,bool is_int) : ICodeValue(ICodeValue::IType::REGISTER) {
  		register_ = r;
  		is_int_ = is_int;
  	}

  	const Register* operator=(const Register& other){
  		register_ = other.val();
  		is_int_ = other.isInt();
  		return this;
  	}

  	int val() const { return register_; }
  	void val(int r) { register_ = r; }

  	bool isInt() const { return is_int_; }
  	bool isFloat() const { return !is_int_; }

  	void setInt() { is_int_ = true; }
  	void setFloat() { is_int_ = false; }

  	bool isValid() const { return val() != -1; }

    void regPrint(ostream& os) const { os << *this; }

    static bool isIntRegister(Type::TypeTag t);
    static bool isFloatRegister(Type::TypeTag t) { return !isIntRegister(t); }
    static bool sameRegisters(const Type* t1,const Type* t2);

  	friend ostream& operator<<(ostream& os,const Register& r);
    friend bool operator==(const Register& r1,const Register& r2);


  private:
  	int register_;
  	bool is_int_;
    //True means int
    //False means float
};

class MemoryMgr {
  public:
	MemoryMgr() {
		base_pointer_value_ = 0;
		stack_pointer_value_ = 0;

		for(int i = 0; i < NUM_REGISTERS;++i){
			int_registers_[i] = false;
			float_registers_[i] = false;
		}

		addRegister(MemoryMgr::stackPointerRegister());
		addRegister(MemoryMgr::basePointerRegister());

    next_int_register_ = 0;
    next_float_register_ = 0;

    while_parent_ = NULL;
	}
	const MemoryMgr* operator=(const MemoryMgr& other){
		base_pointer_value_ = other.base_pointer_value_;
		stack_pointer_value_ = other.stack_pointer_value_;
		for(int i = 0;i < NUM_REGISTERS; ++i){
			int_registers_[i] = other.int_registers_[i];
			float_registers_[i] = other.float_registers_[i];
		}
		return this;
	}

	//True means added and False means not added either because it is already there or out of bounds
	bool addRegister(Register i);

	//Releases the register.  True means register went from occupied to free, false otherwise
	bool freeRegister(Register r);

	//Returns index of first free register DOES NOT RESERVE IT
	//Returns -1 if all are full
	Register getNextRegister(const Type* t);
  Register getNextRegister(bool is_int);

  vector<Register> getUsedRegisters();

	//Outputs the contents of the registers
	void testRegisters() {
		cout << "TEST INT REGISTER" << endl;
		for(int i = 0; i < NUM_REGISTERS;++i){
			cout << int_registers_[i] << " ";
		}
		cout << endl << "TEST FLOAT REGISTER" << endl;
		for(int i = 0; i < NUM_REGISTERS;++i){
			cout << float_registers_[i] << " ";
		}
		cout << endl << "END TEST" << endl;
	}

	int getNextAddress();

	static const Register& stackPointerRegister() { return MemoryMgr::stack_pointer_; }
	static const Register& basePointerRegister() { return MemoryMgr::base_pointer_; }

  WhileNode* whileParent() const { return while_parent_; }
  void whileParent(WhileNode* wp) { while_parent_ = wp; }

 private:
	bool* getTable(bool is_int){
		if(is_int){
			return int_registers_;
		} else {
			return float_registers_;
		}
	}

	int stack_pointer_value_;
	int base_pointer_value_;

	//Last two int registers always have the value of the stack and base pointers
	static const Register stack_pointer_;
	static const Register base_pointer_;

  int next_int_register_;
  int next_float_register_;

	bool int_registers_[NUM_REGISTERS];
	bool float_registers_[NUM_REGISTERS];
  
  //This holds the closest whilenode parent to a statement at any time
  //is Null if no outer while loop    
  WhileNode* while_parent_;
};

#endif
