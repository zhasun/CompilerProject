MOVI 0 R998
MOVI 0 R999
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 37372 R001
MOVI 0 R000
STI R001 R000
ADD R998 1 R998
PRTS "w = "
PRTI R001
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 0 R003
LDI R003 R004
MOVI 12222 R005
ADD R004 R005 R006
MOVI 1 R002
STI R006 R002
ADD R998 1 R998
PRTS "x = "
PRTI R004
PRTS " + "
PRTI R005
PRTS " = "
PRTI R006
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 1 R008
LDI R008 R009
MOVI 2 R007
STI R009 R007
ADD R998 1 R998
PRTS "y = "
PRTI R009
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 0 R011
LDI R011 R012
MOVI 1 R013
LDI R013 R014
ADD R012 R014 R015
MOVIF R015 F000
MOVF 2.753 F001
FSUB F000 F001 F002
MOVI 3 R010
STF F002 R010
ADD R998 1 R998
PRTS "z = "
PRTI R012
PRTS " + "
PRTI R014
PRTS " - "
PRTF F001
PRTS " = "
PRTI R015
PRTS " - "
PRTF F001
PRTS " = "
PRTF F002
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVF 2.45 F003
MOVI 1 R017
LDI R017 R018
MOVI 2 R019
LDI R019 R020
XOR R018 R020 R021
XOR R021 4294967295 R022
MOVI 0 R023
LDI R023 R024
MOVI 1 R025
LDI R025 R026
OR R024 R026 R027
XOR R027 4294967295 R028
AND R022 R028 R029
MOVIF R029 F004
FADD F003 F004 F005
MOVI 4 R016
STF F005 R016
ADD R998 1 R998
PRTS "aaa = "
PRTF F005
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

JMP bool_start__1

bool_start__1:JMP bool_start__0

bool_start__0:JMP bool_true__0

bool_true__0:MOVI 5 R030
STI 1 R030
JMP bool_end__0

bool_false__0:MOVI 5 R030
STI 0 R030

bool_end__0:ADD R998 1 R998
PRTS "cond = "
LDI 5 R878
PRTI R878
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"
JMP parse_start__0

max:PRTS "---  START OF FUNCTION  ---\n\n"
ADD R999 0 R000
LDI R000 R001
ADD R999 1 R002
LDI R002 R003
PRTS "Comparing "
PRTI R001
PRTS " and "
PRTI R003
PRTS "\n\n"
JMPC GT R001 R003 bool_true__1

JMP bool_false__1

bool_true__1:PRTI R001
PRTS " was larger\n\n"
ADD R999 0 R004
LDI R004 R005
ADD R999 3 R006
STI R005 R006
PRTS "RETURNING "
PRTI R005
PRTS "\n\n"
ADD R999 2 R006
LDI R006 R007
JMPI R007

JMP if_end__0

bool_false__1:PRTI R003
PRTS " was larger\n\n"
ADD R999 1 R008
LDI R008 R009
ADD R999 3 R010
STI R009 R010
PRTS "RETURNING "
PRTI R009
PRTS "\n\n"
ADD R999 2 R010
LDI R010 R011
JMPI R011

if_end__0:ADD R999 1 R012
LDI R012 R013
ADD R999 3 R014
STI R013 R014
ADD R999 2 R014
LDI R014 R015
JMPI R015

ADD R999 2 R016
LDI R016 R017
JMPI R017

parse_start__0: PRTS "-----------------\n| Start Parsing |\n-----------------\n\n"
IN R034
PRTS "READ IN VALUE = "
PRTI R034
PRTS "\n\n"
JMPC GT 0 R034 end_program__0

MOVI 97 R035
JMPC EQ R035 R034 rule_start__0

parse_return__0:JMP parse_start__0

rule_start__0:PRTS "RULE START 0\n\n"
ADD R998 0 R000
STI R999 R000
MOVI R998 R999
ADD R998 1 R998
PRTS "SP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

INI R001
PRTS "READ IN INT = "
PRTI R001
PRTS "\n\n"
ADD R999 1 R002
STI R001 R002
ADD R998 1 R998
PRTS "SP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 3 R002

PRTS "STORING USED REGISTERS\n"
STI R002 R998
ADD R998 1 R998
PRTS "DONE STORING USED REGISTERS\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

MOVI 1 R004
LDI R004 R005
ADD R998 0 R003
STI R005 R003
PRTS "First parameter = "
PRTI R005
PRTS "\n\n"


ADD R999 1 R006
LDI R006 R007
ADD R998 1 R003
STI R007 R003
PRTS "Second parameter = "
PRTI R007
PRTS "\n\n"

ADD R998 2 R003
MOVL func_call__0 R008
STI R008 R003

PRTS "SP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

ADD R998 4 R003
STI R999 R003
MOVI R998 R999
MOVI R003 R998
ADD R998 1 R998

PRTS "SP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

JMP max

func_call__0: PRTS "--- RETURN FROM FUNCTION ---\n\n"
ADD R999 3 R003
LDI R003 R009
MOVI R999 R998
ADD R998 4 R003
LDI R003 R999
SUB R998 1 R998
LDI R998 R002
ADD R002 R009 R010
MOVI 10 R011
ADD R010 R011 R012
MOVI 1 R013
STI R012 R013

PRTS "x = "
PRTI R002
PRTS " + "
PRTI R009
PRTS " + "
PRTI R011
PRTS " = "
PRTI R012
PRTS "\nSP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"

ADD R999 0 R000
MOVI R999 R998
LDI R000 R999
PRTS "SP = "
PRTI R998
PRTS ", BP = "
PRTI R999
PRTS "\n\n"
JMP parse_return__0

end_program__0:PRTS "PROGRAM END\n"
