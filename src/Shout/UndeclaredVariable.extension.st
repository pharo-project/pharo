Extension { #name : 'UndeclaredVariable' }

{ #category : '*Shout' }
UndeclaredVariable >> styleNameIn: aVariableNode [

	^ (aVariableNode scope hasBindingThatBeginsWith: self name)
		  ifTrue: [ #incompleteIdentifier ]
		  ifFalse: [ #undefinedIdentifier ]
]
