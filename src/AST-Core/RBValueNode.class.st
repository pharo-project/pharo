"
RBValueNode is an abstract class that represents a node that returns some value.

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
	#name : #RBValueNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'parentheses'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #testing }
RBValueNode class >> isAbstract [

	^ self == RBValueNode
]

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
	"evaluate the AST with a nil  receiver"
	^ self evaluateForReceiver: nil
]

{ #category : #evaluating }
RBValueNode >> evaluateForContext: aContext [
	"evaluate the AST taking variables from the context"
	^(self asDoItForContext: aContext)
		generateWithSource valueWithReceiver: aContext receiver arguments: #()
]

{ #category : #evaluating }
RBValueNode >> evaluateForReceiver: aReceicer [
	"evaluate the AST without taking variables into account"
	| methodNode |

	methodNode := self asDoit.
	methodNode methodClass: aReceicer class.
	^methodNode generateWithSource valueWithReceiver: aReceicer arguments: #()
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
