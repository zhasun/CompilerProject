test/in06
	When printing events with no input, I print out a pair of parentheses.  In the output file no parentheses.  This is the case both in event declarations and in Primitive Patterns.
	Ex:
		Input:
			event fork();
		My output:
			event fork();
		Given Output:
			event fork;

errtest/in2a
	"write" is defined as an event and then used as a function, so my code outputs an error for it.

errtest/in6b
	The event b has fewer arguments than its definition.  My code outputs an error for this, but the given output does not.