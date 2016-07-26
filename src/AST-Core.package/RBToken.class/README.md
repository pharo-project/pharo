RBToken is the abstract superclass of all of the RB tokens. These tokens (unlike the standard parser's) remember where they came from in the original source code.

Subclasses must implement the following messages:
	accessing
		length

Instance Variables:
	sourcePointer	<Integer>	The position in the original source code where this token began.
