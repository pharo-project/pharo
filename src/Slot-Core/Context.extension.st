Extension { #name : #Context }

{ #category : #'*Slot-Core' }
Context >> hasTemporaryVariableNamed: aName [
	^(self tempNames includes: aName)
]

{ #category : #'*Slot-Core' }
Context >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName)
		ifFalse: [ ^ nil ].
	^TemporaryVariable name: aName context: self

]

{ #category : #'*Slot-Core' }
Context >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
