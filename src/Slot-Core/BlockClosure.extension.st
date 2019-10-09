Extension { #name : #BlockClosure }

{ #category : #'*Slot-Core' }
BlockClosure >> hasTemporaryVariableNamed: aName [
	^self tempNames includes: aName
]

{ #category : #'*Slot-Core' }
BlockClosure >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		block: self 

]

{ #category : #'*Slot-Core' }
BlockClosure >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]
