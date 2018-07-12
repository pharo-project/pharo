Class {
	#name : #NewMethodFinderRBNodeVisitor,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitArgumentNode: anArgumentNode [
	 ^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitArgumentNodes: aNodeCollection [
	 ^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitArrayNode: anArrayNode [
	^ ((anArrayNode children select: [:each | self visitNode: each ]) contains: false) not. 
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitAssignmentNode: anAssignmentNode [
	^ self visitNode: anAssignmentNode value
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitBlockNode: aBlockNode [
	^ self visitNode: aBlockNode body
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitCasadeNode: aCascadeNode [
	^ false
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each].
   ^ ((aRBLiteralArrayNode contents select: [:each | self visitNode: each ]) contains: false) not. 
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitLiteralNode: aLiteralNode [
	^ true.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitLiteralVariableNode: aNode [
	^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitMessageNode: aMessageNode [

	^ NewMethodFinder new isSafe: aMessageNode selector.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitMethodNode: aMethodNode [
	self visitNode: aMethodNode body
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitNode: aNode [ 
	^aNode acceptFinderNodeVisitor: self
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitParseErrorNode: anErrorNode [
	^ true.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [ 
	^ (self visitArgumentNodes: aRBPatternBlockNode arguments) and: (self visitNode: aRBPatternBlockNode body).
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [ 
	^ (self visitNode: aRBPatternWrapperBlockNode wrappedNode) and:
	((self visitArgumentNodes: aRBPatternWrapperBlockNode arguments) and:
	(self visitNode: aRBPatternWrapperBlockNode body))
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitPragmaNode: aPragmaNode [
	^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitReturnNode: aReturnNode [ 
	^ self visitNode: aReturnNode value
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitSelfNode: aSelfNode [
	^ true
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitSequenceNode: aSequenceNode [ 
	(self visitTemporaryNodes: aSequenceNode temporaries) and:
	(((aSequenceNode statements select: [:each | self visitNode: each]) contains: false) not)
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitStoreIntoTempNode: aNode [
	^ true.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitStorePopIntoTempNode: aNode [
	^ true.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitTemporaryNodes: aNodeCollection [ 
	"This is triggered when defining the temporaries between the pipes"
	^self visitArgumentNodes: aNodeCollection
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitVariableNode: aVariableNode [
	^ true
]
