Extension { #name : 'CompiledBlock' }

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> ast [
	^ self sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> bcToASTCacheKey [
	"The bcToASTCache of a compiled block is saved in its home method's properties, with its pc in outer code as key.
	So this block is created at pc `48` in its outer code, which is its home method, then its key would be `#48`.
	If this block is created at pc `57` in its outer code, which is itself a block created at pc `48` in its outer code that is its home method, then the key of this block would be `#48>57`"

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
