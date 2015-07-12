#ifndef VALUE_H
#define VALUE_H

#include "Type.h"
#include "ICodeValue.h"

class Value;
class ProgramElem;
class SymTabEntry; 

class Value: public ICodeValue {
 public:
  Value() : ICodeValue(ICodeValue::IType::VALUE)  // void value
    { type_ = &Type::voidType;}
  Value(const char* s) : ICodeValue(ICodeValue::IType::VALUE)
    { sVal_ = new string(s); type_ = &Type::stringType; }
  Value(string s) : ICodeValue(ICodeValue::IType::VALUE)
    { sVal_ = new string(s); type_ = &Type::stringType; }
  Value(bool b) : ICodeValue(ICodeValue::IType::VALUE) { bVal_ = b ; type_ = &Type::boolType; }
  Value(int, Type::TypeTag t);  // For all integral types expressible properly
  Value(unsigned int, Type::TypeTag t);  
  Value(double d) : ICodeValue(ICodeValue::IType::VALUE) // Floats stored as double values
    { type_ = &Type::doubleType; dVal_ = d; }

  Value(Type::TypeTag t) : ICodeValue(ICodeValue::IType::VALUE) { type_ = Type::type[t];  sVal_ = NULL;}
  Value(const Value& v);

  ~Value();

  const Type* type() const { return type_; } 

  string sval() const;
  bool bval() const;
  int ival() const;
  double dval() const;

  void sval(string s) ;
  void bval(bool b) { bVal_ = b ;} 
  void ival(int i) ;
  void dval(double d) ;

  void print(ostream& os, int indent) const;

  void regPrint(ostream& os) const;

  friend bool operator==(const Value& v1,const Value& v2);

 private:
  const Type* type_;       // Shallow-copied from caller of constructor
  union {            // All components of this union shd be deep-copied
    string* sVal_;
    bool bVal_ ;
    int iVal_;
    double dVal_;
  };
};

#endif
