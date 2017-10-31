tests - nodes
testFindBestNodeForTemporaryVariableItsRBVariable
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext tempVarContext .
	self assert: node isTemp.