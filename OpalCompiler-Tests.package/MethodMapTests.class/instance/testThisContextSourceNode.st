testing - ast mapping
testThisContextSourceNode
	self assert: (thisContext sourceNode isKindOf: RBMethodNode).
	self assert: ([thisContext sourceNode] value isKindOf: RBBlockNode).
	self assert: ([true ifTrue: [thisContext sourceNode]]value isKindOf: RBBlockNode).


