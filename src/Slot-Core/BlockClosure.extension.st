Extension { #name : #BlockClosure }

{ #category : #'*Slot-Core' }
BlockClosure >> hasTemporaryVariableNamed: aName [
	^self tempNames includes: aName
]

{ #category : #'*Slot-Core' }
BlockClosure >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^self sourceNode scope lookupVar: aName
]

{ #category : #'*Slot-Core' }
BlockClosure >> temporaryVariables [
	^self sourceNode temporaries collect: [ :each | each binding ] 

]
