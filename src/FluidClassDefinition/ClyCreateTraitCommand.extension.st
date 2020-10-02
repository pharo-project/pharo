Extension { #name : #ClyCreateTraitCommand }

{ #category : #'*FluidClassDefinition' }
ClyCreateTraitCommand >> execute [
	| traitDefinition category resultTrait |
	category := package name.
	classTag ifNotNil: [ category := category , '-' , classTag ].
	traitDefinition := Trait definitionFullyExpandedButPackage: category.
	traitDefinition := UIManager default
		                   multiLineRequest: 'Define trait definition:'
		                   initialAnswer: traitDefinition
		                   answerHeight: 250.
	traitDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := browser
		               compileANewClassFrom: traitDefinition
		               notifying: nil
		               startingFrom: nil.
	resultTrait ifNotNil: [ browser selectClass: resultTrait ]
]
