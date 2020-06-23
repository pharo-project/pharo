Extension { #name : #CompiledMethod }

{ #category : #'*Slot-Core' }
CompiledMethod >> hasTemporaryVariableNamed: aName [
	^ self tempNames includes: aName
]

{ #category : #'*Slot-Core' }
CompiledMethod >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^self sourceNode scope lookupVar: aName


]

{ #category : #'*Slot-Core' }
CompiledMethod >> temporaryVariables [
	^self sourceNode temporaries collect: [ :each | each binding ] 
]
