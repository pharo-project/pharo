Extension { #name : #Context }

{ #category : #'*Slot' }
Context >> hasTemporaryVariableNamed: aName [
	^(self tempNames includes: aName)
]

{ #category : #'*Slot' }
Context >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName)
		ifFalse: [ ^ nil ].
	^TemporaryVariable name: aName context: self

]

{ #category : #'*Slot' }
Context >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
