"
ASTSelectorNode is an AST node that represents a selector (unary, binary, keyword).

Instance Variables:
	value	<String>	the selector's name I represent or the ensemble of keywords I'm made of
	start <Integer>	the position where I was found at the source code

"
Class {
	#name : 'OCSelectorNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'value',
		'keywordPositions'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
OCSelectorNode class >> value: aString [

	^ self value: aString keywordPositions: #(0)
]

{ #category : 'accessing' }
OCSelectorNode class >> value: aString keywordPositions: keywordPositions [

	^ self new
		value: aString;
	 	keywordPositions: keywordPositions;
		yourself
]

{ #category : 'comparing' }
OCSelectorNode >> = anObject [
	self == anObject ifTrue: [^true].
	^ self class = anObject class and: [ self value = anObject value ]
]

{ #category : 'visiting' }
OCSelectorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelectorNode: self
]

{ #category : 'comparing' }
OCSelectorNode >> hash [
	^ self value hash
]

{ #category : 'testing' }
OCSelectorNode >> isSelector [
	^true
]

{ #category : 'accessing' }
OCSelectorNode >> keywordPositions [

	^ keywordPositions
]

{ #category : 'accessing' }
OCSelectorNode >> keywordPositions: positions [

	keywordPositions := positions
]

{ #category : 'accessing' }
OCSelectorNode >> keywords [

	^ value keywords
]

{ #category : 'testing' }
OCSelectorNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
OCSelectorNode >> numArgs [
	^ value numArgs
]

{ #category : 'printing' }
OCSelectorNode >> printOn: aStream [

	aStream
		<< self class name
		<< '('
		<< value asString
		<< ')'
]

{ #category : 'accessing' }
OCSelectorNode >> startWithoutParentheses [
	^ keywordPositions first
]

{ #category : 'accessing' }
OCSelectorNode >> stopWithoutParentheses [

	^ self keywordPositions last + self keywords last size
]

{ #category : 'accessing' }
OCSelectorNode >> value [
	^value
]

{ #category : 'accessing' }
OCSelectorNode >> value: aString [
	value := aString asSymbol
]
