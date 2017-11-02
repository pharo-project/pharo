Extension { #name : #BlockClosure }

{ #category : #'*Slot' }
BlockClosure >> hasTemporaryVariableNamed: aName [
	^(self tempNames includes: aName)
]

{ #category : #'*Slot' }
BlockClosure >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		block: self 

]

{ #category : #'*Slot' }
BlockClosure >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
