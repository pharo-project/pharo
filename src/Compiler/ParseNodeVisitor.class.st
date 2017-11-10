"
I am an abstract superclass for ParseNode visitors that functions as a null visitor.  Here's the code that defines my interface:

(SystemNavigation new allImplementorsOf: #accept: localTo: ParseNode) do:
	[:methodReference|
	methodReference compiledMethod messages do:
		[:sel|
		((sel beginsWith: 'visit')
		and: [sel numArgs = 1]) ifTrue:
			[ParseNodeVisitor
				compile: (String streamContents:
							[:str|
							str nextPutAll: sel;
								space;
								nextPut: $a.
							methodReference classSymbol first isVowel ifTrue:
								[str nextPut: $n].
							str nextPutAll: methodReference classSymbol])
				classified: 'visiting']]]
"
Class {
	#name : #ParseNodeVisitor,
	#superclass : #Object,
	#category : #Compiler-Support
}

{ #category : #visiting }
ParseNodeVisitor >> visitAssignmentNode: anAssignmentNode [
	"N.B.  since assigment happens after the value is evaluated the value is visited first."
	anAssignmentNode value accept: self.
	anAssignmentNode variable accept: self
]

{ #category : #visiting }
ParseNodeVisitor >> visitBlockNode: aBlockNode [
	aBlockNode statements do:
		[:statement| statement accept: self]
]

{ #category : #visiting }
ParseNodeVisitor >> visitBraceNode: aBraceNode [
	aBraceNode elements do:
		[:element| element accept: self]
]

{ #category : #visiting }
ParseNodeVisitor >> visitCascadeNode: aCascadeNode [
	aCascadeNode receiver accept: self.
	aCascadeNode messages do:
		[:message| self visitMessageNodeInCascade: message]
]

{ #category : #visiting }
ParseNodeVisitor >> visitCommentNode: aCommentNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitFieldNode: aFieldNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitInstanceVariableNode: anInstanceVariableNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitLiteralVariableNode: aLiteralVariableNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitMessageNode: aMessageNode [
	aMessageNode receiver accept: self.
	"receiver notNil ifTrue: ''receiver is nil for cascades''
		[receiver accept: self]."
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]
]

{ #category : #visiting }
ParseNodeVisitor >> visitMessageNodeInCascade: aMessageNode [
	"receiver is nil for cascades"
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]
]

{ #category : #visiting }
ParseNodeVisitor >> visitMethodNode: aMethodNode [
	aMethodNode block accept: self
]

{ #category : #visiting }
ParseNodeVisitor >> visitNewArrayNode: aNewArrayNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitRemoteTempVectorNode: aRemoteTempVectorNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitReturnNode: aReturnNode [
	aReturnNode expr accept: self
]

{ #category : #visiting }
ParseNodeVisitor >> visitSelectorNode: aSelectorNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitTempVariableNode: aTempVariableNode [
]

{ #category : #visiting }
ParseNodeVisitor >> visitVariableNode: aVariableNode [
]
