code generation (closures)
constructClosureCreationNode: encoder
	copiedValues := self computeCopiedValues: encoder rootNode.
	encoder supportsClosureOpcodes ifTrue:
		[^self closureCreationNode].
	"Without the bytecode we can still get by."
	^MessageNode new
		receiver: (encoder encodeVariable: 'thisContext')
		selector: #closureCopy:copiedValues:
		arguments: (Array
						with: (encoder encodeLiteral: arguments size)
						with: (copiedValues isEmpty
								ifTrue: [NodeNil]
								ifFalse: [BraceNode new elements: copiedValues]))
		precedence: 3
		from: encoder