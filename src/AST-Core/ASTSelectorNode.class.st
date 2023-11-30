"
RBSelectorNode is an AST node that represents a selector (unary, binary, keyword).

Instance Variables:
	value	<String>	the selector's name I represent or the ensemble of keywords I'm made of
	start <Integer>	the position where I was found at the source code

"
Class {
	#name : 'ASTSelectorNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'value',
		'keywordPositions'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'class initialization' }
ASTSelectorNode class >> initialize [

	self deprecatedAliases: { #ASTSelectorNode }
]

{ #category : 'accessing' }
ASTSelectorNode class >> value: aString [

	^ self value: aString keywordPositions: #(0)
]

{ #category : 'accessing' }
ASTSelectorNode class >> value: aString keywordPositions: keywordPositions [

	^ self new
		value: aString;
	 	keywordPositions: keywordPositions;
		yourself
]

{ #category : 'comparing' }
ASTSelectorNode >> = anObject [
	self == anObject ifTrue: [^true].
	^ self class = anObject class and: [ self value = anObject value ]
]

{ #category : 'visiting' }
ASTSelectorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelectorNode: self
]

{ #category : 'comparing' }
ASTSelectorNode >> hash [
	^ self value hash
]

{ #category : 'testing' }
ASTSelectorNode >> isSelector [
	^true
]

{ #category : 'accessing' }
ASTSelectorNode >> keywordPositions [

	^ keywordPositions
]

{ #category : 'accessing' }
ASTSelectorNode >> keywordPositions: positions [

	keywordPositions := positions
]

{ #category : 'accessing' }
ASTSelectorNode >> keywords [

	^ value keywords
]

{ #category : 'testing' }
ASTSelectorNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
ASTSelectorNode >> numArgs [
	^ value numArgs
]

{ #category : 'printing' }
ASTSelectorNode >> printOn: aStream [

	aStream
		<< self class name
		<< '('
		<< value asString
		<< ')'
]

{ #category : 'accessing' }
ASTSelectorNode >> startWithoutParentheses [
	^ keywordPositions first
]

{ #category : 'accessing' }
ASTSelectorNode >> stopWithoutParentheses [

	^ self keywordPositions last + self keywords last size
]

{ #category : 'accessing' }
ASTSelectorNode >> value [
	^value
]

{ #category : 'accessing' }
ASTSelectorNode >> value: aString [
	value := aString asSymbol
]
