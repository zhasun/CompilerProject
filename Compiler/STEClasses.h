#ifndef STE_CLASSES_H
#define STE_CLASSES_H

#include "SymTab.h"
#include "SymTabEntry.h"
#include "Ast.h"
#include "MemoryMgr.h"
#include "CodeBlock.h"

class StmtNode;
class RuleNode;
class ExprNode;
class PatNode;
class VariableEntry;
class OpNode;
class PrimitivePatNode;
extern string newName(const string&);

/****************************************************************
  The first few classes dont really add any functionality over
  the base class, except for providing a convenient constructor.
****************************************************************/

class GlobalEntry: public SymTabEntry {
 public:
  GlobalEntry(string name, int line=0, int column=0, string file=""):
    SymTabEntry(name, SymTabEntry::Kind::GLOBAL_KIND, line, column,file), rules_() {};
  ~GlobalEntry() {};

  const vector<RuleNode*> rules() const { return rules_;};
  vector<RuleNode*> rules() { return rules_;};
  const RuleNode* rule(int i) const { return rules_[i];}
  RuleNode* rule(int i) { return rules_[i];}
  void addRule(RuleNode* re) { rules_.push_back(re);};

  void print(ostream&, int indent=0) const;
  void typePrint(ostream&, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  CodeBlock* codeGen();
  Type* typeCheck();

 private:
  vector<RuleNode*> rules_;
  MemoryMgr memory_mgr_;

  Label* parse_label_;
  Label* end_label_;

  Register in_reg_;
  Register comp_reg_;

  vector<string> rule_names_;
  vector<Label*> rule_labels_;
  vector<Label*> rule_return_labels_;
  vector<Label*> any_labels_;
  vector<Label*> any_return_labels_;
};

class BlockEntry: public SymTabEntry {
 public:
  BlockEntry(string name, int line=0, int column=0, string file=""):
    SymTabEntry(name, SymTabEntry::Kind::BLOCK_KIND, line, column, file, (Type*)&Type::voidType) {};
  ~BlockEntry() {};
  void print(ostream& out, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
};

class RuleBlockEntry: public BlockEntry {
 public:
  RuleBlockEntry(int line=0, int column=0, string file=""):
    BlockEntry(newName("rule"), line,column, file) { kind(SymTabEntry::Kind::RULE_BLOCK_KIND);};
  ~RuleBlockEntry() {};
};

/****************************************************************
  Following classes add more significant functionality to that
  provided by SymTabEntry.
****************************************************************/

class VariableEntry: public SymTabEntry {
 public:
  enum VarKind {GLOBAL_VAR, LOCAL_VAR, PARAM_VAR, UNDEFINED};

 public:
  VariableEntry(string name, VarKind v, Type* type=nullptr,
				ExprNode* init=nullptr, int ln=0, int col=0, string file=""):
    SymTabEntry(name, SymTabEntry::Kind::VARIABLE_KIND, ln, col, file, type) {
    vkind_ = v; initVal(init); mem_register_ = Register();
    endLabel(NULL);
    trueLabel(NULL);
    falseLabel(NULL);
 };

  VariableEntry(const VariableEntry &v);
  ~VariableEntry() {};

  VarKind varKind() const { return vkind_;};
  void varKind(VarKind v) { vkind_ = v;};

  int offSet() const { return offSet_;} ;
  void offSet(int o) {offSet_ = o;};

  const ExprNode* initVal() const { return initVal_;}
  ExprNode* initVal() { return initVal_;};
  void initVal(ExprNode *init) { initVal_ = init;};

  const Register& reg() const { return mem_register_; }
  void reg(const Register& mr) { mem_register_ = mr; }

  void print(ostream& os, int indent=0) const;
  void typePrint(ostream& os, int indent=0) const;

  void memAlloc(MemoryMgr &mm);
  Type* typeCheck();

  CodeBlock* codeGen();

  Label* endLabel() const { return end_label_; }
  void endLabel(Label* sl) { end_label_ = sl;}

  Label* trueLabel() const { return true_label_; }
  void trueLabel(Label* tl) { true_label_ = tl;}

  Label* falseLabel() const { return false_label_; }
  void falseLabel(Label* fl) { false_label_ = fl;}

 private:
  VarKind vkind_;
  int offSet_;
  ExprNode* initVal_;

  Register mem_register_;

  Label* end_label_;
  Label* true_label_;
  Label* false_label_;
};

class ClassEntry: public SymTabEntry {
 public:
  ClassEntry(string name, int line=0, int column=0, string file="")
    : SymTabEntry(name, SymTabEntry::Kind::CLASS_KIND, line,column, file) {};
  ~ClassEntry() {};

  void print(ostream& os, int indent) const;
  void typePrint(ostream& os, int indent) const;

  void memAlloc(MemoryMgr &sm);

  CodeBlock* codeGen() { cout << "ClassEntry codeGen" << endl; return NULL; }
};

class FunctionEntry: public SymTabEntry {
 public:
  FunctionEntry(string name, Type* type=nullptr,
				int line=0, int column=0, string file=""):
    SymTabEntry(name, SymTabEntry::Kind::FUNCTION_KIND, line,column, file, new Type(Type::TypeTag::FUNCTION)) {
      body_ = nullptr;
      func_start_ = new Label(Label::LabelType::FUNC_DECL,name);
      this->type()->retType(type);
 	};
  ~FunctionEntry() {};

  const CompoundStmtNode* body() const { return body_;};
  CompoundStmtNode* body() {return body_;};
  void body(CompoundStmtNode* n) { body_ = n;};

  Label* funcStart() const { return func_start_; }

  void returnLabel(int return_label) { return_label_ = return_label; }
  int returnLabel() const { return return_label_; }

  void returnOffset(int return_offset) { return_address_ = return_offset; }
  int returnOffset() const { return return_address_; }

  void controlLink(int cl) { control_link_ = cl; }
  int controlLink() const { return control_link_; }

  void print(ostream& os, int indent) const;
  void typePrint(ostream& os, int indent) const;

  void memAlloc(MemoryMgr &mm);
  Type* typeCheck();

  CodeBlock* codeGen();

 private:
  CompoundStmtNode* body_;

  int return_label_;
  int return_address_;
  int control_link_;

  Register label_reg_;
  Register mem_reg_;

  MemoryMgr memory_mgr_;

  Label* func_start_;
};

class EventEntry: public SymTabEntry {
 public:
  EventEntry(string name, int line=0, int column=0, string file=""):
    SymTabEntry(name, SymTabEntry::Kind::EVENT_KIND, line,column, file) {
      type(new Type(Type::TypeTag::EVENT));
    };
  ~EventEntry() {};

  void memAlloc(MemoryMgr &mm);

  void print(ostream& out, int indent=0) const;
  void typePrint(ostream& out, int indent=0) const;

  Type* typeCheck();

  CodeBlock* codeGen() { return NULL; }

 private:
  MemoryMgr memory_mgr_;
};  

#endif
