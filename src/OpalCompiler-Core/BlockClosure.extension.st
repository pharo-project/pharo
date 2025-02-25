Extension { #name : 'BlockClosure' }

{ #category : '*OpalCompiler-Core' }
BlockClosure >> hasTemporaryVariableNamed: aName [
	^self tempNames includes: aName
]

{ #category : '*OpalCompiler-Core' }
BlockClosure >> sourceNode [
	^ self compiledBlock sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
BlockClosure >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^self sourceNode scope lookupVar: aName
]

{ #category : '*OpalCompiler-Core' }
BlockClosure >> temporaryVariables [
	^self sourceNode temporaries collect: [ :each | each binding ]
]
