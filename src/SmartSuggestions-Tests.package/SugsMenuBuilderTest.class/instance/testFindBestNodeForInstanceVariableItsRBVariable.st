tests - nodes
testFindBestNodeForInstanceVariableItsRBVariable
	| node |

	node := SugsMenuBuilder findBestNodeFor: SugsMockContext instVarContext .
	self assert: (node isKindOf: RBVariableNode).
	self assert: (node binding isInstance).