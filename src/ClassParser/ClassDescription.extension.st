Extension { #name : #ClassDescription }

{ #category : #'*ClassParser' }
ClassDescription >> classDefinitionParserClass [
	"Based on the currently selected syntax and the constraints imposed by the class (e.e.g complex slots) return the correct class parser."

	^ (ClassDefinitionPrinter for: self) classDefinitionParserClass
]
