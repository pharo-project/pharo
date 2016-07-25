Holds a unique set of literals.  Literal objects are equal if they are #= plus they are the same class.  This set uses this rule for finding elements.

Example:
	Set new add: 'anthony'; add: #anthony; size  "= 1"
	LiteralSet new add: 'anthony'; add: #anthony; size  "= 2"
