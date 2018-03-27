"
SUnit tests for the Smart suggestions menu builder
"
Class {
	#name : #SugsMenuBuilderTest,
	#superclass : #TestCase,
	#category : #'SmartSuggestions-Tests'
}

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForClassItsRBVariable [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext classContext.
	self assert: (node isKindOf: RBVariableNode).
	"Es una forma un tanto molesta de preguntar... es de clase?" 
	self assert:( node binding isLiteralVariable and: [Smalltalk globals  includesAssociation: node binding assoc ]).
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForClassVarItsRBVariable [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext classVariableContext .
	self assert: (node  isKindOf: RBVariableNode).
	"Es una forma un tanto molesta de preguntar... es una var de clase?" 
	self assert:( node binding isLiteralVariable and: [(Smalltalk globals  includesAssociation: node binding assoc ) not]).
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForInstanceVariableItsRBVariable [
	| node |

	node := SugsMenuBuilder findBestNodeFor: SugsMockContext instVarContext .
	self assert: (node isKindOf: RBVariableNode).
	self assert: (node binding isInstance).
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForLiteralItsRBLiteral [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext literalContext.
	self assert: node class equals: RBLiteralValueNode .
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForMessageNodeItsRBMessageSend [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext messageContext  .
	self assert: node class equals: RBMessageNode .
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForMethodNodeItsRBMehod [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext methodContext .
	self assert: node class equals: RBMethodNode.
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForMultilineItsRBSequence [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext sourceContext .
	self assert: node class equals: RBSequenceNode .

]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForSelfNodeItsRBSelf [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext selfContext.
	self assert: node class equals: RBSelfNode.
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForSelfNodeItsRBSuper [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext superContext.
	self assert: node class equals: RBSuperNode 
]

{ #category : #'tests - nodes' }
SugsMenuBuilderTest >> testFindBestNodeForTemporaryVariableItsRBVariable [
	| node |

	node :=SugsMenuBuilder findBestNodeFor:  SugsMockContext tempVarContext .
	self assert: node isTemp.
]
