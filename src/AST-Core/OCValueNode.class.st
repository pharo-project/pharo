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
	#name : 'OCValueNode',
	#superclass : 'OCProgramNode',
	#instVars : [
		'parentheses'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'testing' }
OCValueNode class >> isAbstract [

	^ self == OCValueNode
]

{ #category : 'accessing' }
OCValueNode >> addParenthesis: anInterval [
	parentheses ifNil: [parentheses := OrderedCollection new: 1].
	parentheses add: anInterval
]

{ #category : 'testing' }
OCValueNode >> containedBy: anInterval [
	^anInterval first <= self startWithoutParentheses
		and: [anInterval last >= self stopWithoutParentheses]
]

{ #category : 'evaluating' }
OCValueNode >> evaluate [
	"evaluate the AST with a nil  receiver"
	^ self evaluateForReceiver: nil
]

{ #category : 'evaluating' }
OCValueNode >> evaluateForContext: aContext [
	"evaluate the AST taking variables from the context"
	^ aContext compiler
		  ast: self asDoit;
		  evaluate
]

{ #category : 'evaluating' }
OCValueNode >> evaluateForReceiver: aReceiver [
	"evaluate the AST binding self to the receiver and taking its variables"

	^ aReceiver class compiler
		  ast: self asDoit;
		  receiver: aReceiver;
		  evaluate
]

{ #category : 'testing' }
OCValueNode >> hasParentheses [
	^self parentheses notEmpty
]

{ #category : 'testing' }
OCValueNode >> isValue [
	^true
]

{ #category : 'testing' }
OCValueNode >> needsParenthesis [
	^self subclassResponsibility
]

{ #category : 'accessing' }
OCValueNode >> parentheses [
	^parentheses ifNil: [#()] ifNotNil: [parentheses]
]

{ #category : 'accessing' }
OCValueNode >> start [
	^ parentheses ifNil: [ self startWithoutParentheses ] ifNotNil: [ parentheses last first ]
]

{ #category : 'accessing' }
OCValueNode >> startWithoutParentheses [
	^self subclassResponsibility
]

{ #category : 'accessing' }
OCValueNode >> stop [
	^ parentheses ifNil: [ self stopWithoutParentheses ] ifNotNil: [ parentheses last last ]
]

{ #category : 'accessing' }
OCValueNode >> stopWithoutParentheses [
	^self subclassResponsibility
]
