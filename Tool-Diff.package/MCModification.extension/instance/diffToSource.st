*Tool-Diff
diffToSource
	"Answer toSource of the modification. If a class patch then answer
	the toSource with the class-side definition and comment appended."
	
	^self isClassPatch
		ifTrue: [self toSource, String cr, String cr, 
				modification classDefinitionString, String cr, String cr,
				modification commentStamp, String cr,
				modification comment]
		ifFalse: [self toSource]