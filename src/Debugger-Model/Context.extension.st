Extension { #name : 'Context' }

{ #category : '*Debugger-Model' }
Context >> privSetClosure: aBlockClosure [ 
	
	closureOrNil := aBlockClosure
]

{ #category : '*Debugger-Model' }
Context >> privSetMethod: aCompiledCode [ 
	
	method := aCompiledCode
]
