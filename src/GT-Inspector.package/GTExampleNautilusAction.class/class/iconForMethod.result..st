private
iconForMethod: aCompiledMethod result: aResult
	aResult
		ifNil: [ ^ Smalltalk ui icons iconNamed: #testNotRunIcon ].
	aResult isSuccess
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #testGreenIcon ].
	aResult isFailure
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #testYellowIcon ].
	aResult isError
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #testRedIcon ].
	^ Smalltalk ui icons iconNamed: #testNotRunIcon