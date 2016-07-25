tests
testVarTemp
	| sequenceNode returnNode |
	sequenceNode := RBParser parseExpression: '| t1 t2 t3 | t1 := 1. t2 := 2. t3 := 3. ^t2'.
	returnNode := sequenceNode statements last.
	
	"before semantic analysis, this is just a variableNode"
	self assert: (returnNode value class == RBVariableNode ).
	self deny: (returnNode value class == RBTemporaryNode).
	
	sequenceNode doSemanticAnalysis.
	"after, it is specialized to a temporary"
	returnNode := sequenceNode statements last.
	self assert: (returnNode value class == RBTemporaryNode).
	