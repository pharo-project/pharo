Extension { #name : #CDSharedVariableNode }

{ #category : #'*Calypso-SystemTools-Core' }
CDSharedVariableNode >> asCalypsoVariableOf: aClass [

	| actualClass classVar |
	actualClass := self classDefinitionNode existingClassIfAbsent: [
		self error: 'Class is not exists yet'].
	classVar := actualClass classVariableNamed: name asSymbol.
	^ClyClassVariable on: classVar definedIn: actualClass
]
