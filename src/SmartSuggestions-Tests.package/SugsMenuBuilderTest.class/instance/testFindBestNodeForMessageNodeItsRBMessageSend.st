tests - nodes
testFindBestNodeForMessageNodeItsRBMessageSend
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext messageContext  .
	self assert: node class equals: RBMessageNode .