"
RBSelectorNode is an AST node that represents a selector (unary, binary, keyword).

Instance Variables:
	value	<String>	the selector's name I represent or the ensemble of keywords I'm made of
	start <Integer>	the position where I was found at the source code

"
Class {
	#name : #RBSelectorNode,
	#superclass : #RBValueNode,
	#instVars : [
		'value',
		'keywordPositions'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #accessing }
RBSelectorNode class >> value: aString [

	^ self value: aString keywordPositions: #(0)
]

{ #category : #accessing }
RBSelectorNode class >> value: aString keywordPositions: keywordPositions [

	^ self new
		value: aString;
	 	keywordPositions: keywordPositions;
		yourself
]

{ #category : #comparing }
RBSelectorNode >> = anObject [
	self == anObject ifTrue: [^true].
	^ self class = anObject class ifTrue: [ self value = anObject value ] ifFalse: [ false ]
]

{ #category : #visiting }
RBSelectorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelectorNode: self
]

{ #category : #comparing }
RBSelectorNode >> hash [
	^ self value hash
]

{ #category : #testing }
RBSelectorNode >> isSelector [
	^true
]

{ #category : #accessing }
RBSelectorNode >> keywordPositions [

	^ keywordPositions
]

{ #category : #accessing }
RBSelectorNode >> keywordPositions: positions [

	keywordPositions := positions
]

{ #category : #accessing }
RBSelectorNode >> keywords [

	^ value keywords
]

{ #category : #testing }
RBSelectorNode >> needsParenthesis [
	^false
]

{ #category : #accessing }
RBSelectorNode >> numArgs [
	^ value numArgs
]

{ #category : #printing }
RBSelectorNode >> printOn: aStream [

	aStream
		<< self class name
		<< '('
		<< value asString
		<< ')'
]

{ #category : #accessing }
RBSelectorNode >> startWithoutParentheses [
	^ keywordPositions first
]

{ #category : #accessing }
RBSelectorNode >> stopWithoutParentheses [

	^ self keywordPositions last + self keywords last size
]

{ #category : #accessing }
RBSelectorNode >> value [
	^value
]

{ #category : #accessing }
RBSelectorNode >> value: aString [
	value := aString asSymbol
]
