Extension { #name : 'ASTProgramNode' }

{ #category : '*DebugPoints' }
ASTProgramNode >> usingMethods [

	^ { self methodNode compiledMethod }
]
