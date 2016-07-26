accessing
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	replacement class == WideString ifTrue: [
		self becomeForward: (WideString from: self).
	]. 

	super replaceFrom: start to: stop with: replacement startingAt: repStart.
