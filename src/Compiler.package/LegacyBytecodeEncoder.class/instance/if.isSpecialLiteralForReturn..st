special literal encodings
if: code isSpecialLiteralForReturn: aBlock
	"If code is that of a special literal for return then evaluate aBlock with the special literal.
	 The special literals for return are nil true false which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil)
	   and: [aBlock value: (#(true false nil) at: code - LdSelf).
			true]