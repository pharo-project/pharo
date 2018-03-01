"
SUnit tests for the Smart suggestions menu builder
"
Class {
	#name : #SugsMenuBuilderTest,
	#superclass : #TestCase,
	#category : #'SmartSuggestions-Tests-Tests'
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

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testNoSuggestionForSelfNode [
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext selfContext.
	expected := SugsSuggestionFactory globalCommands .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testNoSuggestionForSuperNode [
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext superContext.
	expected := SugsSuggestionFactory globalCommands .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAAssigmentNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext assigmentContext.

	expected := SugsSuggestionFactory commandsForAssignment.	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAClassNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext classContext.

	expected := SugsSuggestionFactory commandsForClass.	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAClassVariableNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext classVariableContext.

	expected := SugsSuggestionFactory commandsForClassVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAMehodNodeIncludesAllValid [
	| suggestions expected context |

	context := SugsMockContext methodContext.
	suggestions := SugsMenuBuilder findSuggestionsFor: context.
	expected := (SugsSuggestionFactory commandsForMethod do: [:aSugg | aSugg context: context] )select: #isValidForContext.
	self assert: (expected allSatisfy: [ :expect | suggestions includes: expect ])
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAMessageNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext messageContext.

	expected := SugsSuggestionFactory commandsForMessage .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForATemporaryVariableNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext tempVarContext.

	expected := SugsSuggestionFactory commandsForTemporaryVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForAnInstanceVariableNodeIncludesAllDeclared [
	| suggestions expected|

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext instVarContext.

	expected := SugsSuggestionFactory commandsForInstanceVariable .	
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForLiteralNodeIncludesAllExpected [
	| suggestions expected |

	suggestions :=SugsMenuBuilder findSuggestionsFor: SugsMockContext literalContext.
	expected := SugsSuggestionFactory commandsFoLiteralNode .
	self assert:(expected allSatisfy:[:expect | suggestions includes: expect]).
]

{ #category : #'tests - suggestions by selection' }
SugsMenuBuilderTest >> testSuggestionForSourceNodeIncludesAllValid [
	| suggestions expected context |
	context := SugsMockContext sourceContext.
	suggestions :=SugsMenuBuilder findSuggestionsFor: context.
	expected := (SugsSuggestionFactory commandsForSource do: [:aSugg | aSugg context: context] )select: #isValidForContext.
		
	self assert: (expected allSatisfy:[:expect | suggestions includes: expect]).
]
