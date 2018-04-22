"
RBValueNode is an abstract class that represents a node that returns some value.

Subclasses must implement the following messages:
	accessing
		startWithoutParentheses
		stopWithoutParentheses
	testing
		needsParenthesis

Instance Variables:
	parentheses	<SequenceableCollection of: Inteval>	the positions of the parethesis around this node. We need a collection of intervals for stupid code such as ""((3 + 4))"" that has multiple parethesis around the same expression.


"
Class {
	#name : #RBValueNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'parentheses'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #accessing }
RBValueNode >> addParenthesis: anInterval [ 
	parentheses ifNil: [parentheses := OrderedCollection new: 1].
	parentheses add: anInterval
]

{ #category : #testing }
RBValueNode >> containedBy: anInterval [ 
	^anInterval first <= self startWithoutParentheses 
		and: [anInterval last >= self stopWithoutParentheses]
]

{ #category : #evaluating }
RBValueNode >> evaluate [
	"evaluate the AST without taking variables into account"
	^self asDoit generateWithSource valueWithReceiver: nil arguments: #().


]

{ #category : #evaluating }
RBValueNode >> evaluateForContext: aContext [
	"evaluate the AST taking variables from the context"
	^(self asDoitForContext: aContext) 
		generateWithSource valueWithReceiver: aContext receiver arguments: {aContext}.


]

{ #category : #evaluating }
RBValueNode >> evaluateForReceiver: aReceicer [
	"evaluate the AST without taking variables into account"
	| methodNode |
	
	methodNode := self asDoit.
	methodNode methodClass: aReceicer class.
	^methodNode generateWithSource valueWithReceiver: aReceicer arguments: #().


]

{ #category : #testing }
RBValueNode >> hasParentheses [
	^self parentheses notEmpty
]

{ #category : #testing }
RBValueNode >> isValue [
	^true
]

{ #category : #testing }
RBValueNode >> needsParenthesis [
	^self subclassResponsibility
]

{ #category : #accessing }
RBValueNode >> parentheses [
	^parentheses ifNil: [#()] ifNotNil: [parentheses]
]

{ #category : #accessing }
RBValueNode >> start [
	^ parentheses ifNil: [ self startWithoutParentheses ] ifNotNil: [ parentheses last first ]
]

{ #category : #accessing }
RBValueNode >> startWithoutParentheses [
	^self subclassResponsibility
]

{ #category : #accessing }
RBValueNode >> stop [
	^ parentheses ifNil: [ self stopWithoutParentheses ] ifNotNil: [ parentheses last last ]
]

{ #category : #accessing }
RBValueNode >> stopWithoutParentheses [
	^self subclassResponsibility
]
