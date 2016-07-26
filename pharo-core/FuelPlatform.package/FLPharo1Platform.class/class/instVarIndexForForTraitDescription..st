private-extensions-traitdescription
instVarIndexForForTraitDescription: instVarName
	<flExtensionOf: #TraitDescription selector: #instVarIndexFor:>
	"Answer the index of the named instance variable."

	| index |
	index := instanceVariables == nil 
		ifTrue: [0]
		ifFalse: [instanceVariables indexOf: instVarName].
	index = 0 ifTrue: 
		[^superclass == nil 
			ifTrue: [0]
			ifFalse: [superclass instVarIndexFor: instVarName]].
	^superclass == nil 	
		ifTrue: [index]
		ifFalse: [index + superclass instSize]