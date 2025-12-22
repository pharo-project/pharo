Extension { #name : 'CompiledCode' }

{ #category : '*Deprecated14-Kernel' }
CompiledCode class >> newMethod: numberOfBytes header: headerWord [

	self 
		deprecated: 'Use #basicNew:header: instead.' 
		transformWith: '`@rcv newMethod: `@numberOfBytes header: `@headerWord' -> '`@rcv basicNew: `@numberOfBytes header: `@headerWord'.
	
	^ self basicNew: numberOfBytes header: headerWord
]
