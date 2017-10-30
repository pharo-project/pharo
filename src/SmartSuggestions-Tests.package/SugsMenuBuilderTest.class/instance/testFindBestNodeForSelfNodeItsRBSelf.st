tests - nodes
testFindBestNodeForSelfNodeItsRBSelf
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext selfContext.
	self assert: node class equals: RBSelfNode.