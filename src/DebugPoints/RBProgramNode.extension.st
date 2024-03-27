Extension { #name : 'RBProgramNode' }

{ #category : '*DebugPoints' }
RBProgramNode >> usingMethods [

	^ { self methodNode compiledMethod }
]
