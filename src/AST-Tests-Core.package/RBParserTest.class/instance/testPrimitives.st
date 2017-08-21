tests parsing
testPrimitives
	self assert: (Object parseTreeFor: #basicAt:) isPrimitive.
	#(('foo ^true' false ) ('foo <some: #tag> ^true' false ) (' foo <some: #tag> <primitive: 123> ^true' true ) ) do: [:each | self assert: (self parserClass parseMethod: each first) isPrimitive = each last]