private
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	replacement isString
		ifFalse:
			[super replaceFrom: start to: stop with: replacement startingAt: repStart]
		ifTrue:
			[ "use String>>byteAt: to mimic prim 105"
			| index repOff |
			repOff := repStart - start.
			index := start - 1.
			[(index := index + 1) <= stop]
				whileTrue: [self at: index put: (replacement byteAt: repOff + index)]]
