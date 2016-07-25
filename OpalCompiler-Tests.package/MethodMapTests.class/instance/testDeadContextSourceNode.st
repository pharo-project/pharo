testing - ast mapping
testDeadContextSourceNode
	| deadContext |
	deadContext := self deadContext.
	self assert: deadContext isDead. 
	self assert: deadContext sourceNode equals: (self class>>#deadContext) ast
