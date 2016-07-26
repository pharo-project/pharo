nodes
testFindBestNodeForMessageNodeItsRBMessageSend
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext mesageContext  .
	self assert: node class equals: RBMessageNode .