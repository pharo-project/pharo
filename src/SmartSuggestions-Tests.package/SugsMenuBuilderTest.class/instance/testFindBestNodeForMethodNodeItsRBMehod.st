tests - nodes
testFindBestNodeForMethodNodeItsRBMehod
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext methodContext .
	self assert: node class equals: RBMethodNode.