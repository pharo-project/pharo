tests-printing
testPrintingObject

	| actual |
	actual := MirrorPrimitives print: 0@0.
	
	self assert: actual equals: 'a Point'