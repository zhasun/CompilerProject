import argparse
import struct

parser = argparse.ArgumentParser(description='Input Heyawake Puzzles')
parser.add_argument('outFile',metavar='O',type=str,nargs=1,help='Output File')
parser.add_argument('events',metavar='E',type=str,nargs='*',default = [])

args = parser.parse_args()
ofile = args.outFile[0]
events = args.events

f = open(ofile,'w')

def print_int(v):
	f.write(struct.pack('<I',v))

def print_float(v):
	f.write(struct.pack('<L',v))

for e in events:
	n,other = e.split('(')
	e_args = other[:-1].split(',')
	f.write(n)
	for a in e_args:
		if '.' in a:
			v = float(a)
			print_float(v)
		else:
			v = int(a)
			print_int(v)

f.close()

