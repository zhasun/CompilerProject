#ifndef _CODE_BLOCK_H_
#define _CODE_BLOCK_H_

#include "all.h"
#include "MemoryMgr.h"
#include "ICodeValue.h"

#define NUM_ICODETYPE 44


struct cmp_reg {
	bool operator()(const Register* a,const Register* b){
		if(a->isInt() && b->isFloat()){
			return true;
		}
		if(b->isInt() && a->isFloat()){
			return false;
		}
		return a->val() > b->val();
	}
};
class ICode;

typedef map<Register*,vector<ICode*>,cmp_reg> def_map;

//Intermediate Code statements
class ICode: public ICodeValue{
  public:
	enum class ICodeType { //Don't change the order
		NONE = -1,
		ADD,SUB,DIV,MUL,MOD,AND,OR,XOR,
		FADD,FSUB,FDIV,FMUL,
		NEG,FNEG,
		UGT,UGE,GT,GE,EQ,NE,FGT,FGE,FEQ,FNE,
		PRTS,PRTI,PRTF,
		JMP,JMPC,JMPI,JMPCI,
		MOVL,MOVS,MOVI,MOVF,
		MOVIF,MOVFI,
		STI,STF,
		LDI,LDF,
		IN,INI,INF
	};

	ICode() : ICodeValue(ICodeValue::IType::ICODE) { type_ = ICodeType::NONE; }
	ICode(ICodeType ict,const ICodeValue* icv1,const ICodeValue* icv2 = NULL,const ICodeValue* icv3 = NULL);
	ICode(const ICode& ic)  : ICodeValue(ICodeValue::IType::ICODE) {
		type_ = ic.type();
		values_ = ic.values_;
	}

	ICodeType type() const { return type_; }

	bool isJump() const {
		return type() == ICodeType::JMP || type() == ICodeType::JMPC || type() == ICodeType::JMPI || type() == ICodeType::JMPCI;
	}
	bool isDirectJump() const {
		return type() == ICodeType::JMP || type() == ICodeType::JMPC;
	}
	bool isConditionalJump() const {
		return type() == ICodeType::JMPC || type() == ICodeType::JMPCI;
	}
	bool isArithmetic() const {
		return type() == ICodeType::ADD || type() == ICodeType::SUB || type() == ICodeType::DIV || type() == ICodeType::MUL || type() == ICodeType::MOD || type() == ICodeType::AND || type() == ICodeType::OR || type() == ICodeType::XOR || type() == ICodeType::FADD || type() == ICodeType::FSUB || type() == ICodeType::FDIV || type() == ICodeType::FMUL || type() == ICodeType::NEG || type() == ICodeType::FNEG;
	}
	bool isComparison() const {
		return type() == ICodeType::UGT || type() == ICodeType::UGE || type() == ICodeType::GT || type() == ICodeType::GE || type() == ICodeType::EQ || type() == ICodeType::NE || type() == ICodeType::FGT || type() == ICodeType::FGE || type() == ICodeType::FEQ || type() == ICodeType::FNE;
	}
	bool isMove() const {
		return type() == ICodeType::MOVL || type() == ICodeType::MOVS || type() == ICodeType::MOVI || type() == ICodeType::MOVF || type() == ICodeType::MOVIF || type() == ICodeType::MOVFI;
	}
	bool isStore() const {
		return type() == ICodeType::STI || type() == ICodeType::STF;
	}
	bool isLoad() const {
		return type() == ICodeType::LDI || type() == ICodeType::LDF;
	}
	bool isInput() const {
		return type() == ICodeType::IN || type() == ICodeType::INI || type() == ICodeType::INF;
	}
	bool isPrint() const {
		return type() == ICodeType::PRTS || type() == ICodeType::PRTI || type() == ICodeType::PRTF;
	}

	static ICodeType getMovType(const Register& r1,const Register& r2){
      if(r1.isInt() && r2.isFloat()){
        return ICodeType::MOVIF;
      }
      return ICodeType::MOVFI;
    }

    ICode* getCondition(){
    	if(!isConditionalJump()){
    		return NULL;
    	}
    	return (ICode*)values_[0];
    }

	Label* getLabel() const {
		if(type() == ICodeType::JMP){
			return (Label*)values_[0];
		}
		if(type() == ICodeType::JMPC){
			return (Label*)values_[1];
		}
		return NULL;
	}
	void setLabel(Label* l){
		if(type() == ICodeType::JMP){
			values_[0] = l;
		}
		if(type() == ICodeType::JMPC){
			values_[1] = l;
		}
	}

	Register* getDefinitionRegister(){
		if(isArithmetic() || isLoad() || isInput() || isMove()){
			const ICodeValue* pos_r = values_[values_.size()-1];
			if(pos_r->itype() == ICodeValue::IType::REGISTER){
				return (Register*)pos_r;
			}
		}
		return NULL;
	}
	vector<Register*> getUseRegisters();

	bool isUseful(){
		if(isArithmetic() || isMove() || isLoad()){
			if(uses_.size() != 0){
				return true;
			}
			if(getDefinitionRegister() != NULL && (*getDefinitionRegister() == MemoryMgr::stackPointerRegister() || *getDefinitionRegister() == MemoryMgr::basePointerRegister())){
				return true;
			}

			return false;
		}
		return true;
	}

	vector<ICodeValue*> getUseValues();

	void makeDUChains();
	void clearMetaData(){
		uses_.clear();
		defs_.clear();
		definitions_.clear();
	}
	bool constantProgation();
	bool commonSubExpression();
	bool replaceReg(Register* r ,ICodeValue* v,ICode* ic);
	bool commonSubReplaceReg(Register* r_org ,Register* r_replace,ICode* ic_org,ICode* ic_replace);

	pair<int,bool> getAddress();

	void defMap(const def_map& d){ definitions_ = d; }
	const def_map& defMap() { return definitions_; }

	void print(ostream &os) const  {
		regPrint(os);
		os << endl;
	}


	void addUse(ICode* ic) { uses_.push_back(ic); }

	void regPrint(ostream &os) const;
	// Can either create enumeration to show type
	// or create a bunch of subclasses
  private:
  	static int arity_[NUM_ICODETYPE];
  	static string names_[NUM_ICODETYPE];

  	vector<ICode*> uses_; //ICodes that use this definition
  	def_map defs_; //Definitions that are used in this line

  	vector<const ICodeValue*> values_;

  	ICodeType type_;

  	def_map definitions_; // All definitions that are valid at this point
};

//Block of intermediate code
class CodeBlock {
  public:
  	CodeBlock();
	CodeBlock(ICode ic);
	~CodeBlock() {}

	CodeBlock* nextBlock() const { return next_block_; }
	void nextBlock(CodeBlock* nb) { next_block_ = nb; }

	void append(CodeBlock* cb);
	void append(ICode ic);

	CodeBlock* jumpBlock() const { return jump_block_; }
	void jumpBlock(CodeBlock* jb) { jump_block_ = jb; }

	void addParent(CodeBlock* p) {parents_.push_back(p); }

	void setStartLabel(Label* sl);
	void setEndLabel(Label* sl);

	bool pushDefinitions();

	void defMap(const def_map& d){ definitions_ = d; }
	bool changeDefMap(def_map& d) { return d != definitions_; }
	def_map defMap() const { return definitions_; }

	bool straightJump() const {
		if(code_.size() <= 0){
			return false;
		}
		ICode last = code_[code_.size()-1];
		return last.type() == ICode::ICodeType::JMP || last.type() == ICode::ICodeType::JMPI;
	}

	const ICode* getJump() const {
		if(code_.size() <= 0 || !ends_with_jump_){
			return NULL;
		}
		return &code_[code_.size()-1];
	}

	void popLastLine();

	void Optimize();
	bool removeUselessStatements();
	void setMetaData();
	void connectJumps();
	void clearMetaData();
	void makeDUChains();
	bool constantProgation();
	bool commonSubExpression();
	bool jumpOptimizations();

	vector<CodeBlock*> getIncoming() const;

	void print(ostream& os);
  private:
  	//Block can only have one label at the very beginning
  	vector<Label*> start_label_;

  	vector<ICode> code_;

  	def_map definitions_;

  	//Blocks that jump to this
  	vector<CodeBlock*> parents_;
  	//Block immediately before it
  	CodeBlock* prev_block_;


  	//Block immediately after it
  	CodeBlock* next_block_;
  	//Block that is jumped to
  	CodeBlock* jump_block_;

  	bool ends_with_jump_;
  	bool starts_with_label_;

  	// Stores pointers for labels and where they are
  	static vector<pair<CodeBlock*,Label*> > label_locs;
  	// Stores where direct jumps are
  	static vector<pair<CodeBlock*,Label*> > jump_locs;
};

#endif
