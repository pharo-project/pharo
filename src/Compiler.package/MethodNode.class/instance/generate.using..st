as yet unclassified
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically either the reference to the source code
	 that is stored with every CompiledMethod, or an encoding of the method's temporary names."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := trailer
					createMethod: blkSize
					class: aCompiledMethodClass
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method