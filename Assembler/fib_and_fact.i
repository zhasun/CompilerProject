MOVI 0 R998
MOVI 0 R999
MOVI 1 R998
MOVI 2 R998
MOVI 3 R998
JMP parse_start__0

fib:ADD R999 0 R000
LDI R000 R001
JMPC GE 1 R001 bool_true__0

JMP if_end__0

bool_true__0:ADD R999 2 R004
STI 1 R004
ADD R999 1 R004
LDI R004 R005
JMPI R005

if_end__0:ADD R999 0 R007
LDI R007 R008
SUB R008 1 R010
ADD R998 0 R006
STI R010 R006
ADD R998 1 R006
MOVL func_call__0 R011
STI R011 R006
ADD R998 3 R006
STI R999 R006
MOVI R998 R999
MOVI R006 R998
ADD R998 1 R998
JMP fib

func_call__0:ADD R999 2 R006
LDI R006 R012
MOVI R999 R998
ADD R998 3 R006
LDI R006 R999
STI R012 R998
ADD R998 1 R998
ADD R999 0 R014
LDI R014 R015
SUB R015 2 R017
ADD R998 0 R013
STI R017 R013
ADD R998 1 R013
MOVL func_call__1 R018
STI R018 R013
ADD R998 3 R013
STI R999 R013
MOVI R998 R999
MOVI R013 R998
ADD R998 1 R998
JMP fib

func_call__1:ADD R999 2 R013
LDI R013 R019
MOVI R999 R998
ADD R998 3 R013
LDI R013 R999
SUB R998 1 R998
LDI R998 R012
ADD R012 R019 R020
ADD R999 2 R021
STI R020 R021
ADD R999 1 R021
LDI R021 R022
JMPI R022

ADD R999 1 R023
LDI R023 R024
JMPI R024

parse_start__0:IN R000
JMPC GT 0 R000 end_program__0

JMPC EQ 97 R000 rule_start__0

parse_return__0:JMPC EQ 98 R000 rule_start__1

parse_return__1:JMP parse_start__0

rule_start__0:ADD R998 0 R001
STI R999 R001
MOVI R998 R999
ADD R998 1 R998
INI R002
ADD R999 1 R003
STI R002 R003
ADD R998 1 R998
STI R000 R998
ADD R998 1 R998
LDI R003 R006
ADD R998 0 R004
STI R006 R004
ADD R998 1 R004
MOVL func_call__2 R007
STI R007 R004
ADD R998 3 R004
STI R999 R004
MOVI R998 R999
MOVI R004 R998
ADD R998 1 R998
JMP fib

func_call__2:ADD R999 2 R004
LDI R004 R008
MOVI R999 R998
ADD R998 3 R004
LDI R004 R999
SUB R998 1 R998
STI R008 0
LDI 0 R011
ADD R999 1 R012
LDI R012 R013
ADD R011 R013 R014
PRTS "The "
PRTI R013
PRTS "th value is "
PRTI R011
PRTS "\n"
ADD R999 0 R001
MOVI R999 R998
LDI R001 R999
JMP parse_return__0

rule_start__1:ADD R998 0 R001
STI R999 R001
MOVI R998 R999
ADD R998 1 R998
INI R002
ADD R999 1 R003
STI R002 R003
ADD R998 1 R998
STI 1 1
STI 1 2

while_begin__0:LDI 2 R009
LDI R003 R011
JMPC GE R011 R009 while_body__0

JMP while_end__0

while_body__0:LDI 1 R013
MUL R013 R009 R016
STI R016 1
ADD R009 1 R021
STI R021 2
JMP while_begin__0

while_end__0:LDI 1 R024
ADD R024 R011 R027
PRTS "Factorial of "
PRTI R024
PRTS " is "
PRTI R011
PRTS "\n"
ADD R999 0 R001
MOVI R999 R998
LDI R001 R999
JMP parse_return__1

end_program__0:PRTS "PROGRAM END\n"