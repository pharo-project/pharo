"
ASTValueNode is an abstract class that represents a node that returns some value.

Subclasses must implement the following messages:
	accessing
		startWithoutParentheses
		stopWithoutParentheses
	testing
		needsParenthesis

Instance Variables:
	parentheses	<SequenceableCollection of: Inteval>	the positions of the parentheses around this node. We need a collection of intervals for stupid code such as ""((3 + 4))"" that has multiple parentheses around the same expression.


"
Class {
	#name : #ASTValueNode,
	#superclass : #ASTProgramNode,
	#instVars : [
		'parentheses'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #testing }
ASTValueNode class >> isAbstract [
		
	^ self == ASTValueNode
]

{ #category : #accessing }
ASTValueNode >> addParenthesis: anInterval [ 
	parentheses ifNil: [parentheses := OrderedCollection new: 1].
	parentheses add: anInterval
]

{ #category : #testing }
ASTValueNode >> containedBy: anInterval [ 
	^anInterval first <= self startWithoutParentheses 
		and: [anInterval last >= self stopWithoutParentheses]
]

{ #category : #evaluating }
ASTValueNode >> evaluate [
	"evaluate the AST with a nil  receiver"
	^ self evaluateForReceiver: nil
]

{ #category : #evaluating }
ASTValueNode >> evaluateForContext: aContext [
	"evaluate the AST taking variables from the context"
	^(self asDoitForContext: aContext) 
		generateWithSource valueWithReceiver: aContext receiver arguments: {aContext}.
]

{ #category : #evaluating }
ASTValueNode >> evaluateForReceiver: aReceicer [
	"evaluate the AST without taking variables into account"
	| methodNode |
	
	methodNode := self asDoit.
	methodNode methodClass: aReceicer class.
	^methodNode generateWithSource valueWithReceiver: aReceicer arguments: #().
]

{ #category : #testing }
ASTValueNode >> hasParentheses [
	^self parentheses notEmpty
]

{ #category : #testing }
ASTValueNode >> isValue [
	^true
]

{ #category : #testing }
ASTValueNode >> needsParenthesis [
	^self subclassResponsibility
]

{ #category : #accessing }
ASTValueNode >> parentheses [
	^parentheses ifNil: [#()] ifNotNil: [parentheses]
]

{ #category : #accessing }
ASTValueNode >> start [
	^ parentheses ifNil: [ self startWithoutParentheses ] ifNotNil: [ parentheses last first ]
]

{ #category : #accessing }
ASTValueNode >> startWithoutParentheses [
	^self subclassResponsibility
]

{ #category : #accessing }
ASTValueNode >> stop [
	^ parentheses ifNil: [ self stopWithoutParentheses ] ifNotNil: [ parentheses last last ]
]

{ #category : #accessing }
ASTValueNode >> stopWithoutParentheses [
	^self subclassResponsibility
]
