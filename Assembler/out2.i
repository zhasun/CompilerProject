MOVI 0 R998
MOVI 0 R999

MOVI 37372 R001
MOVI 0 R000
STI R001 R000
MOVI 100 R111
STI 10 100
LDI 100 R111
PRTS "TEST: "
PRTI R111
PRTS "\n"
PRTS "w = "
PRTI R001
PRTS "\n"
ADD R998 1 R998
PRTS "SP = "
PRTI R998
PRTS "\n"

MOVI 0 R003
LDI R004 R003
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
PRTS "\n"
PRTS "SP = "
PRTI R998
PRTS "\n"