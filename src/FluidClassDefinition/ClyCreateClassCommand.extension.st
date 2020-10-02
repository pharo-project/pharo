Extension { #name : #ClyCreateClassCommand }

{ #category : #'*FluidClassDefinition' }
ClyCreateClassCommand >> execute [
	| classDefinition p resultTrait |
	p := package name.
	classTag ifNotNil: [ p := p , '-' , classTag ].
	classDefinition := ClassDescription definitionFullyExpandedButPackage: p.
	classDefinition := UIManager default
		                   multiLineRequest: 'Define class definition:'
		                   initialAnswer: classDefinition
		                   answerHeight: 250.
	classDefinition isEmptyOrNil ifTrue: [ ^ self ].
	resultTrait := browser
		               compileANewClassFrom: classDefinition
		               notifying: nil
		               startingFrom: nil.
	resultTrait ifNotNil: [ browser selectClass: resultTrait ]
]
