Extension { #name : 'BlockClosure' }

{ #category : '*OpalCompiler-Core' }
BlockClosure >> sourceNode [
	^ self compiledBlock sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
BlockClosure >> temporaryVariables [
	^self sourceNode temporaries collect: [ :each | each binding ]
]
