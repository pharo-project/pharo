"
An ASTComment represents a text comment associated with an AST node.

An ASTComment is not an AST-Node (not a subclass of program node). But its instances are just wrapping the comment text and (start-) position.

Due to the way the parser handles comments, the ASTComment is assigned to its preceding (real) AST node, although we often write the comment prior to a statement.

For example:

foo
""method comment""

self firstStatement.

""comment about the return""
^ self

The ""method comment"" is assigned to the method node, the ""comment about the return"" is assigned
to the ""self firstStatement"" node!

instance variables
	contents 	<String> the comment text
	start	<Number> (start-) position within the method source

"
Class {
	#name : #ASTComment,
	#superclass : #ASTProgramNode,
	#instVars : [
		'contents',
		'start',
		'stop'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
ASTComment class >> with: aCommentToken [
	"To respect the previously implemented comment node content, 
	 we strip the surrounding quotes of the comment."
	^ self new
		with: aCommentToken value from: aCommentToken start to: aCommentToken stop;
		yourself.
]

{ #category : #'instance creation' }
ASTComment class >> with: aString at: startPosition [

	^ self with: (ASTCommentToken
			   value: aString
			   start: startPosition
			   stop: startPosition + aString size - 1)
]

{ #category : #comparing }
ASTComment >> = anObject [
	self == anObject ifTrue: [ ^true ].
	^ self class = anObject class ifTrue: [ self contents = anObject contents ] ifFalse: [ false ]
]

{ #category : #visiting }
ASTComment >> acceptVisitor: aProgramNodeVisitor [
	"At some point we will have to think what we do to visit comment. 
	It may have an impact on visitors so this should be done carefully.
	Since by default previously comment node were not subclass of ProgramNode 
	we do nothing by default."
]

{ #category : #accessing }
ASTComment >> contents [
	^ contents
]

{ #category : #comparing }
ASTComment >> hash [
	^ self contents hash
]

{ #category : #testing }
ASTComment >> isCommentNode [ 
	^true
]

{ #category : #printing }
ASTComment >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' "'.
	aStream nextPutAll: contents.
	aStream nextPutAll: '" '.
]

{ #category : #enumeration }
ASTComment >> size [
	^ contents size + 2 "must take into account quotation marks"
]

{ #category : #accessing }
ASTComment >> start [
	^ start
]

{ #category : #accessing }
ASTComment >> stop [
	^ stop ifNil: [ start + self size - 1 ]
			 ifNotNil: [ stop ].
]

{ #category : #'instance creation' }
ASTComment >> with: aString from: aStartPosition to: aStopPosition [
	start := aStartPosition.
	stop := aStopPosition.
	contents := aString.
]
