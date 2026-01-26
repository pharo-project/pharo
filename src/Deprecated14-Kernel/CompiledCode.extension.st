Extension { #name : 'CompiledCode' }

{ #category : '*Deprecated14-Kernel' }
CompiledCode >> accessesRef: literalVariable [
	"Answer whether this method accesses the LiteralVariable"

	"we do not need to call readsRef: or #writesRef:, as if the variable
	is stored as a Literal, it will be accessed. This check is much faster"

	self
		deprecated: 'Use #hasLiteral: instead since the name it more accurate'
		transformWith: '`@rcv accessesRef: `@arg' -> '`@rcv hasLiteral: `@arg'.
	^ self hasLiteral: literalVariable 
]

{ #category : '*Deprecated14-Kernel' }
CompiledCode class >> newMethod: numberOfBytes header: headerWord [

	self 
		deprecated: 'Use #basicNew:header: instead.' 
		transformWith: '`@rcv newMethod: `@numberOfBytes header: `@headerWord' -> '`@rcv basicNew: `@numberOfBytes header: `@headerWord'.
	
	^ self basicNew: numberOfBytes header: headerWord
]
