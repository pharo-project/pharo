*Tool-Diff
diffFromSource
	"Answer fromSource of the modification. If a class patch then answer
	the fromSource with the class-side definition and comment appended."
	
	^self isClassPatch
		ifTrue: [self fromSource, String cr, String cr,
				obsoletion classDefinitionString, String cr, String cr,
				obsoletion commentStamp, String cr,
				obsoletion comment]
		ifFalse: [obsoletion diffSource]