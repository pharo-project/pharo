Check for assignments on block arguments.
For example:

[:x :y|
	x:= x+y.
	]

The block argument "x" should not be written. This is considered bad style. And some compiler may reject that code.