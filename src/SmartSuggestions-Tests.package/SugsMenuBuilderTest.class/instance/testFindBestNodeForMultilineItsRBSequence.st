tests - nodes
testFindBestNodeForMultilineItsRBSequence
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext sourceContext .
	self assert: node class equals: RBSequenceNode .
