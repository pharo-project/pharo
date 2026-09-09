Extension { #name : 'OCProgramNode' }

{ #category : '*DebugPoints' }
OCProgramNode >> usingMethods [

	^ { self methodNode compiledMethod }
]
