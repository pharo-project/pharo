Extension { #name : 'BlockClosure' }

{ #category : '*OpalCompiler-Core' }
BlockClosure >> sourceNode [
	^ self compiledBlock sourceNodeInOuter
]
