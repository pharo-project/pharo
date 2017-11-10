Extension { #name : #CompiledBlock }

{ #category : #'*OpalCompiler-Core' }
CompiledBlock >> ast [
	^ self sourceNodeInOuter
]

{ #category : #'*OpalCompiler-Core' }
CompiledBlock >> sourceNode [
	^ self sourceNodeInOuter
]

{ #category : #'*OpalCompiler-Core' }
CompiledBlock >> sourceNodeExecutedForPC: aPC [
	| blockNode |
	blockNode := self outerCode sourceNodeExecutedForPC: self pcInOuter.
	^blockNode sourceNodeExecutedForPC: aPC
]

{ #category : #'*OpalCompiler-Core' }
CompiledBlock >> sourceNodeForPC: aPC [
	| blockNode |
	blockNode := self outerCode sourceNodeForPC: self pcInOuter.
	^blockNode sourceNodeForPC: aPC
]

{ #category : #'*OpalCompiler-Core' }
CompiledBlock >> sourceNodeInOuter [
	^ self outerCode sourceNodeForPC: self pcInOuter
]
