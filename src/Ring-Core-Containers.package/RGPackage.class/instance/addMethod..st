adding/removing
addMethod: aRGMethodDefinition
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse:[ ^self ].
	aRGMethodDefinition package: self.	"Needed to evaluate #isExtension if it was not set in advance"
	self 
		addElement: aRGMethodDefinition 
		in: (aRGMethodDefinition isExtension 
			ifTrue: [ self extensionMethods ]
			ifFalse:[ super methods ])