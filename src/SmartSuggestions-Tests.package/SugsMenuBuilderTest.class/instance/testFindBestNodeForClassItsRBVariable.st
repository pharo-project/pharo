tests - nodes
testFindBestNodeForClassItsRBVariable
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext classContext.
	self assert: (node isKindOf: RBVariableNode).
	"Es una forma un tanto molesta de preguntar... es de clase?" 
	self assert:( node binding isLiteralVariable and: [Smalltalk globals  includesAssociation: node binding assoc ]).