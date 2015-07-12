#ifndef _ICODEVALUE_H_
#define _ICODEVALUE_H_

#include "all.h"

class ICodeValue {
  public:
  	enum class IType {
  		REGISTER,
  		VALUE,
  		LABEL,
  		ICODE
  	};

  	ICodeValue(IType it){
  		i_type_ = it;
  	}

  	virtual void regPrint(ostream& os) const = 0;

  	IType itype() const { return i_type_; }

private:
	IType i_type_;
};

#endif
