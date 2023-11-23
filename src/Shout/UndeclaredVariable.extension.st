Extension { #name : 'UndeclaredVariable' }

{ #category : '*Shout' }
UndeclaredVariable >> styleNameIn: aRBVariableNode [

	^ (aRBVariableNode scope hasBindingThatBeginsWith: self name)
		  ifTrue: [ #incompleteIdentifier ]
		  ifFalse: [ #undefinedIdentifier ]
]
