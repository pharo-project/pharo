adding/removing
addMethod: aRGMethodDefinition
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse:[ ^self ].
	self addElement: aRGMethodDefinition in: self methods