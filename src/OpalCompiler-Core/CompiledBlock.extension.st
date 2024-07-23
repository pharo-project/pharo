Extension { #name : 'CompiledBlock' }

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> ast [
	^ self sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> bcToASTCacheKey [

	| key outerCode |
	key := self pcInOuter asString.
	outerCode := self outerCode.

	outerCode isCompiledBlock ifTrue: [
		key := '{1}>{2}' format: {
				       outerCode pcInOuter.
				       key }.
		outerCode := outerCode outerCode ].
	^ key
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> sourceNode [
	^ self sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> sourceNodeForPC: aPC [

	| blockNode |
	blockNode := self outerCode sourceNodeForPC: self pcInOuter.
	"Bug in the cache? The mapping is returning Return node instead of Block"
	blockNode isReturn ifTrue: [ blockNode := blockNode value ].
	self method
		propertyAt: self bcToASTCacheKey asSymbol
		ifPresent: [ :bcToASTCache |
			blockNode sourceNodeForPC: aPC usingBcToASTCache: bcToASTCache ].
	^ blockNode sourceNodeForPC: aPC
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> sourceNodeInOuter [
	^ self outerCode sourceNodeForPC: self pcInOuter
]
