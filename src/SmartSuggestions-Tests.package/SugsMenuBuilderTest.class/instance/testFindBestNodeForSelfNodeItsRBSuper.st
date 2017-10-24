tests - nodes
testFindBestNodeForSelfNodeItsRBSuper
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext superContext.
	self assert: node class equals: RBSuperNode 