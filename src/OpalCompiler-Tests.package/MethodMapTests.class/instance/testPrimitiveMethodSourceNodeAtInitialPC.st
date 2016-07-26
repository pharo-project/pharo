testing - ast mapping
testPrimitiveMethodSourceNodeAtInitialPC

	| method actual |
	method := SmallInteger >> #+.
	actual := method sourceNodeForPC: method initialPC.
	
	self assert: actual equals: method ast
