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
	"on the level of compiledMethod, temps include the arguments"
	self flag: #TODO. "we need to revist temps vs args"
	^(self sourceNode arguments collect: [ :each | each binding ]), 
		(self sourceNode temporaries collect: [ :each | each binding ])
]
