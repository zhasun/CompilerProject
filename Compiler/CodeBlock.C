#include "CodeBlock.h"
#include "Value.h"
#include "Type.h"
#include "MemoryMgr.h"

int ICode::arity_[NUM_ICODETYPE] = {
	3,3,3,3,3,3,3,3,
	3,3,3,3,
	2,2,
	2,2,2,2,2,2,
	2,2,2,2,
	1,1,1,
	1,2,1,2,
	2,2,2,2,
	2,2,
	2,2,
	2,2,
	1,1,1
};
string ICode::names_[NUM_ICODETYPE] = {
	"ADD","SUB","DIV","MUL","MOD","AND","OR","XOR",
	"FADD","FSUB","FDIV","FMUL",
	"NEG","FNEG",
	"UGT","UGE","GT","GE","EQ","NE",
	"FGT","FGE","FEQ","FNE",
	"PRTS","PRTI","PRTF",
	"JMP","JMPC","JMPI","JMPCI",
	"MOVL","MOVS","MOVI","MOVF",
	"MOVIF","MIVFI",
	"STI","STF",
	"LDI","LDF",
	"IN","INI","INF"
};

//Checks arity of input and specified type
//Could add check to make sure values are of right type
ICode::ICode(ICodeType ict,const ICodeValue* icv1,const ICodeValue* icv2,const ICodeValue* icv3)  : ICodeValue(ICodeValue::IType::ICODE) {
	const ICodeValue* icvs[3] = {icv1,icv2,icv3};
	for(int x = 0;x<3;++x){
		if(icvs[x]!=NULL){
			values_.push_back(icvs[x]);
		}
	}

	type_ = ict;
	int i = (int)type_;

	if(values_.size() != (unsigned int)arity_[i]){
		type_ = ICodeType::NONE;
	}
}

void ICode::regPrint(ostream& os) const {
	int i = (int)type();
	if(i==-1){
		os << "NONE "<< values_.size() << endl;
		return;
	}
	os << names_[i] << " ";
	for(unsigned int x = 0;x < values_.size();++x){
		if(values_[x] != NULL){
			values_[x]->regPrint(os);
		} else {
			os << "NULLVALUE" << endl;
		}
		if(x+1 < values_.size()){
			os << " ";
		}
	}

	//constant, register, label, or comparison icode statements(for conditional jumps)
}

void ICode::makeDUChains(){
	vector<Register*> u_regs = getUseRegisters();
	for(unsigned int i = 0; i < u_regs.size(); i++){
		vector<ICode*> use_places = definitions_[u_regs[i]];
		defs_[u_regs[i]]= use_places;
		for(unsigned int j = 0; j < use_places.size(); j++){
			use_places[j]->addUse(this);
		}
	}
}

bool ICode::constantProgation(){
	Register* d_reg = getDefinitionRegister();
	vector<ICodeValue*> u_vals = getUseValues();
	if(u_vals.size() == 0 || d_reg == NULL){
		return false;
	}
	for(unsigned int i = 0;i < u_vals.size(); i++){
		if(u_vals[i]->itype() != ICodeValue::IType::VALUE){
			return false;
		}
	}
	if(isArithmetic()){
		Value* v1 = NULL,*v2 = NULL;
		Register* d_r = NULL;
		for(unsigned int i = 0; i < values_.size(); ++i){
			if(values_[i]->itype() == ICodeValue::IType::VALUE){
				if(v1 == NULL){
					v1 = (Value*)values_[i];
				} else {
					v2 = (Value*)values_[i];
				}
			}
			if(values_[i]->itype() == ICodeValue::IType::REGISTER){
				d_r = (Register*)values_[i];
			}
		}

		Value* new_v = NULL;
		ICode::ICodeType new_type = ICode::ICodeType::MOVI;
		if(type() == ICodeType::ADD){
			new_v = new Value(v1->ival() + v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::SUB){
			new_v = new Value(v1->ival() - v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::DIV){
			new_v = new Value(v1->ival() / v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::MUL){
			new_v = new Value(v1->ival() * v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::MOD){
			new_v = new Value(v1->ival() % v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::AND){
			new_v = new Value(v1->ival() & v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::OR){
			new_v = new Value(v1->ival() | v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::XOR){
			new_v = new Value(v1->ival() ^ v2->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::FADD){
			new_v = new Value(v1->dval() + v2->dval());
			new_type = ICode::ICodeType::MOVF;
		}
		if(type() == ICodeType::FSUB){
			new_v = new Value(v1->dval() - v2->dval());
			new_type = ICode::ICodeType::MOVF;
		}
		if(type() == ICodeType::FDIV){
			new_v = new Value(v1->dval() / v2->dval());
			new_type = ICode::ICodeType::MOVF;
		}
		if(type() == ICodeType::FMUL){
			new_v = new Value(v1->dval() * v2->dval());
			new_type = ICode::ICodeType::MOVF;
		}
		if(type() == ICodeType::NEG){
			new_v = new Value(-v1->ival(),Type::TypeTag::INT);
		}
		if(type() == ICodeType::FNEG){
			new_v = new Value(-v1->dval());
			new_type = ICode::ICodeType::MOVF;
		}

		if(new_v == NULL){
			return false;
		}

		values_.clear();
		values_.push_back(new_v);
		values_.push_back(d_r);
		type_ = new_type;
		return true;
	}else if(isMove()){
		Value* v = (Value*)u_vals[0];
		if(type() == ICode::ICodeType::MOVIF){
			v = new Value((double)v->ival());
		}
		if(type() == ICode::ICodeType::MOVFI){
			v = new Value((int)v->dval(),Type::TypeTag::INT);
		}
		for(unsigned int i = 0; i < uses_.size();++i){
			if(uses_[i]->replaceReg(d_reg,v,this)){
				return true;
				uses_.erase(uses_.begin()+i);
				i--;
			}
		}
	}
	return false;
}

bool ICode::commonSubExpression(){
	if(isJump() || isStore() || isComparison() || isInput() || isPrint()){
		return false;
	}
	vector<ICodeValue*> u_vals = getUseValues();
	int xx = 0;
	for(def_map::iterator itr = definitions_.begin(); itr != definitions_.end(); itr++){
		xx++;
		for(unsigned int i = 0; i < itr->second.size(); i++){
			//Same use values(either VALUE or REGISTER)
			//If register then same and only one def
			vector<ICodeValue*> pos_u_vals = itr->second[i]->getUseValues();
			if(u_vals.size() != pos_u_vals.size() || type() != itr->second[i]->type()){
				continue;
			}
			if(this == itr->second[i]){
				continue;
			}
			Register* d_this = getDefinitionRegister();
			Register* d_other = itr->second[i]->getDefinitionRegister();
			bool replace = false;

			//Check if the two subexpressions are the same
			if(u_vals.size() == 1){
				ICodeValue* v_this = (ICodeValue*)values_[0];
				ICodeValue* v_other = (ICodeValue*)itr->second[i]->values_[0];
				if(v_this->itype() == ICodeValue::IType::VALUE && v_other->itype() == ICodeValue::IType::VALUE){
					Value* vv_this = (Value*)v_this;
					Value* vv_other = (Value*)v_other;
					if(*vv_this == *vv_other){
						replace = true;
					}
				}
				if(v_this->itype() == ICodeValue::IType::REGISTER && v_other->itype() == ICodeValue::IType::REGISTER){
					Register* vr_this = (Register*)v_this;
					Register* vr_other = (Register*)v_other;
					if(defs_[vr_this].size() == 1 && itr->second[i]->defs_[vr_other].size() == 1){
						ICode* def_this = defs_[vr_this][0];
						ICode* def_other = itr->second[i]->defs_[vr_other][0];
						if(*vr_this == *vr_other && def_this == def_other){
							replace = true;
						}
					}					
				}
			}

			if(u_vals.size() == 2){
				for(int k =0; k < 2 ; ++k){
					ICodeValue* v_this1 = (ICodeValue*)values_[0];
					ICodeValue* v_this2 = (ICodeValue*)values_[1];
					ICodeValue* v_other1 = (ICodeValue*)itr->second[i]->values_[k];
					ICodeValue* v_other2 = (ICodeValue*)itr->second[i]->values_[(k+1)%2];
					if(v_this1->itype() == ICodeValue::IType::VALUE && v_other1->itype() == ICodeValue::IType::VALUE && v_this2->itype() == ICodeValue::IType::VALUE && v_other2->itype() == ICodeValue::IType::VALUE){
						Value* vv_this1 = (Value*)v_this1;
						Value* vv_other1 = (Value*)v_other1;
						Value* vv_this2 = (Value*)v_this2;
						Value* vv_other2 = (Value*) v_other2;
						if(*vv_this1 == *vv_other1 && *vv_this2 == *vv_other2){
							replace = true;
							break;
						}
					}
					if(v_this1->itype() == ICodeValue::IType::REGISTER && v_other1->itype() == ICodeValue::IType::REGISTER && v_this2->itype() == ICodeValue::IType::VALUE && v_other2->itype() == ICodeValue::IType::VALUE){
						Register* vr_this1 = (Register*)v_this1;
						Register* vr_other1 = (Register*)v_other1;
						Value* vv_this2 = (Value*)v_this2;
						Value* vv_other2 = (Value*) v_other2;
						if(defs_[vr_this1].size() == 1 && itr->second[i]->defs_[vr_other1].size() == 1){
							ICode* def_this1 = defs_[vr_this1][0];
							ICode* def_other1 = itr->second[i]->defs_[vr_other1][0];
							if(*vr_this1 == *vr_other1 && *vv_this2 == *vv_other2 && def_this1 == def_other1){
								replace = true;
								break;
							}
						}
						
					}
					if(v_this1->itype() == ICodeValue::IType::VALUE && v_other1->itype() == ICodeValue::IType::VALUE && v_this2->itype() == ICodeValue::IType::REGISTER && v_other2->itype() == ICodeValue::IType::REGISTER){
						Value* vv_this1 = (Value*)v_this1;
						Value* vv_other1 = (Value*)v_other1;
						Register* vr_this2 = (Register*)v_this2;
						Register* vr_other2 = (Register*)v_other2;
						if(defs_[vr_this2].size() == 1 && itr->second[i]->defs_[vr_other2].size() == 1){
							ICode* def_this2 = defs_[vr_this2][0];
							ICode* def_other2 = itr->second[i]->defs_[vr_other2][0];
							
							if(*vv_this1 == *vv_other1 && *vr_this2 == *vr_other2 && def_this2 == def_other2){
								replace = true;
								break;
							}
						}
					}
					if(v_this1->itype() == ICodeValue::IType::REGISTER && v_other1->itype() == ICodeValue::IType::REGISTER && v_this2->itype() == ICodeValue::IType::REGISTER && v_other2->itype() == ICodeValue::IType::REGISTER){
						Register* vr_this1 = (Register*)v_this1;
						Register* vr_other1 = (Register*)v_other1;
						Register* vr_this2 = (Register*)v_this2;
						Register* vr_other2 = (Register*)v_other2;
						if(defs_[vr_this2].size() == 1 && itr->second[i]->defs_[vr_other2].size() == 1 && defs_[vr_this1].size() == 1 && itr->second[i]->defs_[vr_other1].size() == 1){
							ICode* def_this1 = defs_[vr_this1][0];
							ICode* def_other1 = itr->second[i]->defs_[vr_other1][0];
							ICode* def_this2 = defs_[vr_this2][0];
							ICode* def_other2 = itr->second[i]->defs_[vr_other2][0];
							
							if(*vr_this1 == *vr_other1 && *vr_this2 == *vr_other2 && def_this2 == def_other2 && def_this1 == def_other1){
								replace = true;
								break;
							}
						}
					}
				}
			}
			if(replace){
				for(unsigned int j = 0; j < uses_.size(); j++){
					if(uses_[j]->commonSubReplaceReg(d_this,d_other,this,itr->second[i])){
						itr->second[i]->addUse(uses_[j]);
						uses_.erase(uses_.begin()+j);
						j--;
						return true;
					}
				}
			}
		}
	}
	return false;
}

pair<int,bool> ICode::getAddress(){
	if(!(isStore() || isLoad())){
		return make_pair(-1,false);
	}

	ICodeValue* val = NULL;
	if(isStore()){
		val = (ICodeValue*)values_[1];
	} else {
		val = (ICodeValue*)values_[0];
	}
	
	if(val->itype() == ICodeValue::IType::VALUE){
		Value* vval = (Value*) val;
		return make_pair(vval->ival(),false);
	} else if(val->itype() == ICodeValue::IType::REGISTER){
		Register* rval = (Register*) val;
		vector<ICode*> use_places = definitions_[rval];
		int m = -1;
		bool from_bp = false,set = false;
		for(unsigned int i = 0; i < use_places.size();i++){
			int ind = -1;
			bool t_from_bp = false;
			if(use_places[i]->type() == ICode::ICodeType::MOVI && use_places[i]->values_[0]->itype() == ICodeValue::IType::VALUE){
				ind = 0;
				t_from_bp = false;
			}
			if(use_places[i]->type() == ICode::ICodeType::ADD && use_places[i]->values_[0]->itype() == ICodeValue::IType::VALUE && use_places[i]->values_[1]->itype() == ICodeValue::IType::REGISTER){
				if(*((Register*)use_places[i]->values_[1]) == MemoryMgr::basePointerRegister()){
					ind = 0;
					t_from_bp = true;
				}
			}
			if(use_places[i]->type() == ICode::ICodeType::ADD && use_places[i]->values_[1]->itype() == ICodeValue::IType::VALUE && use_places[i]->values_[0]->itype() == ICodeValue::IType::REGISTER){
				Register* pos_bp = ((Register*)use_places[i]->values_[0]);
				if(*(pos_bp) == MemoryMgr::basePointerRegister()){
					ind = 1;
					t_from_bp = true;
				}
			}
			if(ind != -1){
				Value* vval = (Value*) use_places[i]->values_[ind];
				if(!set){

					m = vval->ival();
					from_bp = t_from_bp;
					set = true;
				} else {
					if(vval->ival() != m || from_bp != t_from_bp){
						return make_pair(-1,false);
					}
				}
			} else {
				return make_pair(-1,false);
			}
		}
		return make_pair(m,from_bp);
	}
	return make_pair(-1,false);
}

bool ICode::replaceReg(Register* r,ICodeValue* v,ICode* ic){
	//Makes sure this line uses this register
	def_map::iterator itr = defs_.find(r);
	if(itr == defs_.end()){
		return false;
	}
	//Checks to see if this register can only be defined from ic
	vector<ICode*> d_places = defs_[r];
	unsigned int pos = -1;
	for(unsigned int j = 0; j < d_places.size();++j){
		if(ic != d_places[j]){
			return false;
		}
		if(ic == d_places[j]){
			pos = j;
		}
	}

	if(isJump()){
		if(isConditionalJump()){
			bool change = false;
			vector<ICodeValue*> u_vals = getUseValues();
			for(unsigned int i = 0;i < u_vals.size(); i++){
				if(u_vals[i]->itype() == ICodeValue::IType::REGISTER){
					Register* u_r = (Register*)u_vals[i];
					if(*r == *u_r){

						((ICode*)values_[0])->values_[i] = (const ICodeValue*)v;
						change = true;
						defs_[r].erase(defs_[r].begin()+pos);
					}
				}
			}
			return change;
		}
		return false;
	}
	
	//Replaces value
	vector<ICodeValue*> u_vals = getUseValues();
	bool change = false;
	for(unsigned int i = 0;i < u_vals.size(); i++){
		if(u_vals[i]->itype() == ICodeValue::IType::REGISTER){
			Register* u_r = (Register*)u_vals[i];
			if(*r == *u_r){
				values_[i] = (const ICodeValue*)v;
				if(!change){
					defs_[r].erase(defs_[r].begin()+pos);
				}
				change = true;
			}
		}
	}
	return change;
}

bool ICode::commonSubReplaceReg(Register* r_org ,Register* r_replace,ICode* ic_org,ICode* ic_replace){
	// //Checks to see if this register can only be defined from ic_replace
	vector<ICode*> d_places = definitions_[r_replace];
	for(unsigned int j = 0; j < d_places.size();++j){
		if(ic_replace != d_places[j]){
			return false;
		}
	}
	return replaceReg(r_org,new Register(*r_replace),ic_org);
}

vector<Register*> ICode::getUseRegisters(){
	vector<Register*> u_regs;
	vector<ICodeValue*> u_vals = getUseValues();
	for(unsigned int i = 0; i < u_vals.size(); i++){
		if(u_vals[i]->itype() == ICodeValue::IType::REGISTER){
			u_regs.push_back((Register*)u_vals[i]);
		}
	}
	return u_regs;
}

vector<ICodeValue*> ICode::getUseValues(){
	vector<ICodeValue*> u_vals;
	unsigned int n = 0;
	if(isArithmetic() || isLoad() || isInput() || isMove()){
		n = values_.size() - 1;
	}
	if(isStore() || isComparison()){
		n = values_.size();
	}
	if(isJump()){
		vector<ICodeValue*> vs;
		if(isConditionalJump()){
			if(values_[0]->itype() == ICodeValue::IType::ICODE){
				vs = ((ICode*)values_[0])->getUseValues();
			} else {
				vs.push_back((ICodeValue*)values_[0]);
			}
		}
		if(!isDirectJump()){
			vs.push_back((ICodeValue*)values_[values_.size()-1]);
		}
		return vs;		
	}

	for(unsigned int i = 0; i < n; i++){
		u_vals.push_back((ICodeValue*)values_[i]);
	}
	return u_vals;
}

vector<pair<CodeBlock*,Label*> > CodeBlock::label_locs;
vector<pair<CodeBlock*,Label*> > CodeBlock::jump_locs;

CodeBlock::CodeBlock() {
	ends_with_jump_ = false;
	starts_with_label_ = false;
	next_block_ = NULL;
	prev_block_ = NULL;
	jump_block_ = NULL;
}

CodeBlock::CodeBlock(ICode ic){
	code_.push_back(ic);
	ends_with_jump_ = ic.isJump();
	starts_with_label_ = false;
	next_block_ = NULL;
	prev_block_ = NULL;
	jump_block_ = NULL;
}

void CodeBlock::append(CodeBlock* cb){
	if(cb == NULL){
		return;
	}
	if(next_block_ == NULL){
		if(this->ends_with_jump_ || cb->starts_with_label_){
			this->next_block_ = cb;
			cb->prev_block_ = this;
			return;
		} else {
			this->ends_with_jump_ = cb->ends_with_jump_;
			for(unsigned int  i = 0; i < cb->code_.size();++i){
				this->code_.push_back(cb->code_[i]);
			}
			if(cb->next_block_ != NULL){
				this->next_block_ = cb->next_block_;
				cb->next_block_->prev_block_ = this;
			}
			delete cb;
		}
	} else {
		next_block_->append(cb);
	}
}

void CodeBlock::append(ICode ic){
	if(next_block_ == NULL){
		if(this->ends_with_jump_){
			CodeBlock* cb = new CodeBlock(ic);
			this->next_block_ = cb;
			cb->prev_block_ = this;
			return;
		} else {
			this->code_.push_back(ic);
			if(ic.isJump()){
				this->ends_with_jump_ = true;
			}
		}
	} else {
		next_block_->append(ic);
	}
}

void CodeBlock::setStartLabel(Label* sl) {
	start_label_.push_back(sl);
	starts_with_label_ = true;
}

void CodeBlock::setEndLabel(Label* el){
	if(nextBlock() != NULL){
		nextBlock()->setEndLabel(el);
		return;
	}
	CodeBlock* e_block = new CodeBlock();
	e_block->setStartLabel(el);
	this->next_block_ = e_block;
	e_block->prev_block_ = this;
}

void CodeBlock::connectJumps(){
	if(starts_with_label_){
		for(unsigned int i = 0; i < start_label_.size(); i ++){
			label_locs.push_back(make_pair(this,start_label_[i]));
			for(unsigned int j = 0; j < jump_locs.size(); ++j){
				if(*start_label_[i] == *(jump_locs[j].second)){
					jump_locs[j].first->jumpBlock(this);
					this->parents_.push_back(jump_locs[j].first);
				}
			}
		}
	}
	if(ends_with_jump_){
		Label* jl = code_[code_.size()-1].getLabel();
		if(jl != NULL){
			jump_locs.push_back(make_pair(this,jl));
			for(unsigned int i = 0; i < label_locs.size(); i ++){
				if(*jl == *(label_locs[i].second)){
					this->jumpBlock(label_locs[i].first);
					label_locs[i].first->addParent(this);
				}
			}
		}
	}
	if(next_block_!= NULL){
		next_block_->connectJumps();
	}
}

bool CodeBlock::pushDefinitions(){
	//Get all incoming definitions and union them
	vector<CodeBlock*> incoming = getIncoming();
	def_map defs;
	//Union of incoming definitions
	for(unsigned int x = 0; x < incoming.size(); ++x){
		def_map incoming_def = incoming[x]->defMap();
		for(def_map::iterator itr = incoming_def.begin(); itr != incoming_def.end(); itr++){
			for(unsigned int y = 0; y < itr->second.size(); y++){
				bool add = true;
				for(unsigned int z = 0;z < defs[itr->first].size();z++){
					if(itr->second[y] == defs[itr->first][z]){
						add = false;
					}
				}
				if(add){
					defs[itr->first].push_back(itr->second[y]);
				}
			}
		}
	}
	bool change = false;
	//Going through each Icode
	for(unsigned int i = 0; i < code_.size() ; i++){
		change = (defs != code_[i].defMap()) || change;
		code_[i].defMap(defs);
		Register * r_def = code_[i].getDefinitionRegister();
		if(r_def != NULL){
			def_map::iterator itr = defs.find(r_def);
			if(itr != defs.end()){
				defs[r_def].clear();
			}
			defs[r_def].push_back(&code_[i]);
		}
		if(code_[i].isStore()){
			pair<int,bool> store_address = code_[i].getAddress();
			for(def_map::iterator itr = defs.begin(); itr != defs.end(); itr++){
				for(unsigned int k = 0; k < itr->second.size(); ++k){
					if(itr->second[k]->isLoad()){
						pair<int,bool> load_address = itr->second[k]->getAddress();
						if(store_address.first == -1 || load_address.first == -1 || store_address == load_address){
							itr->second.erase(itr->second.begin()+k);
							k--;
						}
					}
				}
			}
		}

	}
	// const ICode* jmp = getJump();
	// if(jmp == NULL || !jmp->isDirectJump() || (jmp->getLabel()->labelType() != Label::LabelType::PARSE_RETURN)) {
		change = changeDefMap(defs) || change;
		defMap(defs); // Stores for end of codeBlock
	if(next_block_ != NULL){
		change = next_block_->pushDefinitions() || change;
	}
	return change;
}

void CodeBlock::Optimize(){
	setMetaData();
	bool any_change = true;
	while(any_change){
		any_change = false;
		bool change = true;
	    while(change){
	    	change = jumpOptimizations();
	    	setMetaData();
	    	any_change = any_change || change;
	    }
	    change = true;
	    while(change){
	    	change = constantProgation();
	    	change = removeUselessStatements() || change;
	    	setMetaData();
	    	any_change = any_change || change;
	    }
	    change = true;
	    while(change){
	    	change = commonSubExpression();
	    	change = removeUselessStatements() || change;
	    	setMetaData();
	    	any_change = any_change || change;
	    }
	}
}

bool CodeBlock::removeUselessStatements(){
	bool change = false;
	for(vector<ICode>::iterator itr = code_.begin();itr != code_.end();itr++){
		if(!(itr->isUseful())){
			change = true;
			code_.erase(itr);
			--itr;
		}
	}
	if(next_block_ != NULL){
		change = next_block_->removeUselessStatements() || change;
	}
	return change;
}

void CodeBlock::setMetaData(){
	label_locs.clear();
	jump_locs.clear();
	clearMetaData();
	connectJumps();
	bool push = true;
    while(push){
      push = pushDefinitions();
    }
    makeDUChains();
}

void CodeBlock::clearMetaData(){
	for(unsigned int i = 0; i<code_.size();++i){
		code_[i].clearMetaData();
	}
	definitions_.clear();
	parents_.clear();
	jump_block_ = NULL;
	if(next_block_ != NULL){
		next_block_->clearMetaData();
	}
}

void CodeBlock::makeDUChains(){
	for(unsigned int i = 0; i< code_.size();++i){
		code_[i].makeDUChains();
	}
	if(next_block_ != NULL){
		next_block_->makeDUChains();
	}
}

bool CodeBlock::constantProgation(){
	for(unsigned int i = 0; i< code_.size();++i){
		if(code_[i].constantProgation()){
			return true;
		}
	}
	bool change = false;
	if(next_block_ != NULL){
		change = next_block_->constantProgation() || change;
	}
	return change;
}

bool CodeBlock::commonSubExpression(){
	bool change = false;
	for(unsigned int i = 0; i< code_.size();++i){
		change = code_[i].commonSubExpression();
		if(change){
			return true;
		}
	}
	if(next_block_ != NULL){
		change = next_block_->commonSubExpression() || change;
	}
	return change;
}

void CodeBlock::popLastLine(){
	code_.erase(code_.begin() + (code_.size()-1));
}

bool CodeBlock::jumpOptimizations(){
	if(code_.size() == 0 && next_block_ != NULL){
		for(unsigned int i = 0; i < start_label_.size(); i++){
			next_block_->setStartLabel(start_label_[i]);
		}
		prev_block_->next_block_ = this->next_block_;
		next_block_->prev_block_ = this->prev_block_;
		delete this;
		return true;
	}

	if(jump_block_ != NULL){
		ICode* jmp = (ICode*)getJump();
		if(jmp != NULL && jmp->isDirectJump()){
			Label* jl = jmp->getLabel();
			if(jl != NULL && jump_block_->start_label_.size() >=2){
			 	if(!(*jump_block_->start_label_[0] == *jl)){
			 		jmp->setLabel(jump_block_->start_label_[0]);
			 		return true;
			 	}
			}
		}
	}

	if(prev_block_ != NULL){
		const ICode* jmp = prev_block_->getJump();
		if(jmp != NULL && jmp->isDirectJump()){
			Label* jl = jmp->getLabel();
			for(unsigned int i = 0; i < start_label_.size(); i++){
				if(*jl == *start_label_[i]){
					prev_block_->popLastLine();
					return true;
				}
			}
		}
	}

	if(next_block_ != NULL){
		return next_block_->jumpOptimizations();
	}
	return false;
}

vector<CodeBlock*> CodeBlock::getIncoming() const {
	for(unsigned int i = 0; i < start_label_.size(); ++i){
		if(start_label_[i]->labelType() == Label::LabelType::RULE_START || start_label_[i]->labelType() == Label::LabelType::FUNC_DECL){
			vector<CodeBlock*> x;
			return x;
		}
	}
	if(prev_block_ == NULL || prev_block_->straightJump()){
		return parents_;
	}
	vector<CodeBlock*> incoming = parents_;
	incoming.push_back(prev_block_);
	return incoming;
}

void CodeBlock::print(ostream& os) {
	if(prev_block_!=NULL){
		os << endl;
	}
	for(unsigned int i = 0; i < start_label_.size();++i){
		os << *(start_label_[i]);
		break;
	}
	for(unsigned int i = 0; i < code_.size();++i){
		code_[i].print(os);
	}
	if(nextBlock() != NULL){
		nextBlock()->print(os);
	}
}
