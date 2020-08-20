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
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #accessing }
RBSelectorNode class >> value: aString [
	^ self value: aString start: #(0)
]

{ #category : #accessing }
RBSelectorNode class >> value: aString start: anInteger [
	^ self new value: aString ;
	           start: anInteger.
]

{ #category : #comparing }
RBSelectorNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	^ self class = anObject class ifTrue: [ self value = anObject value ] ifFalse: [ false ]
]

{ #category : #comparing }
RBSelectorNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelectorNode: self
]

{ #category : #comparing }
RBSelectorNode >> hash [
	^ self value hash
]

{ #category : #comparing }
RBSelectorNode >> isSelector [
	^true
]

{ #category : #comparing }
RBSelectorNode >> needsParenthesis [
	^false
]

{ #category : #'as yet unclassified' }
RBSelectorNode >> numArgs [ 
	^value numArgs.
]

{ #category : #'as yet unclassified' }
RBSelectorNode >> printOn: aStream [

	aStream
		<< self class name
		<< '('
		<< value asString
		<< ')'
]

{ #category : #comparing }
RBSelectorNode >> start: aPositionsList [
	start := aPositionsList ifNotNil: [:list| list asIntegerArray ].
]

{ #category : #comparing }
RBSelectorNode >> startWithoutParentheses [
	^ start
]

{ #category : #comparing }
RBSelectorNode >> stopWithoutParentheses [
	| temp prev |
	temp := 0.
	^ (start collect: [ :each | prev := temp. 
							 temp := value 
								findString: ':' 
								startingAt: prev+1. 
							 temp=0 
								ifFalse: [each + temp - prev -1 ] 
								ifTrue: [each + value size - 1]]) asIntegerArray.
	
]

{ #category : #comparing }
RBSelectorNode >> value [
	^value
]

{ #category : #comparing }
RBSelectorNode >> value: aString [
	value := aString asSymbol
]
