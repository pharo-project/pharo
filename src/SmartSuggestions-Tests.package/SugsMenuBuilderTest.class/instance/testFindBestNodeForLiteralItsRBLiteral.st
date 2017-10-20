tests - nodes
testFindBestNodeForLiteralItsRBLiteral
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext literalContext.
	self assert: node class equals: RBLiteralValueNode .