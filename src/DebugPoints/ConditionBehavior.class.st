"
I am a behavior defining an execution condition that can be added to debug points. 
If my condition evaluates to true or nil, the #execute method will return true, else it will return false or an error.
"
Class {
	#name : 'ConditionBehavior',
	#superclass : 'DebugPointBehavior',
	#instVars : [
		'condition'
	],
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'accessing' }
ConditionBehavior >> condition [
	^condition
]

{ #category : 'accessing' }
ConditionBehavior >> condition: aCondition [
	condition := aCondition
]

{ #category : 'accessing' }
ConditionBehavior >> context [
	^self getArgument: #context
]

{ #category : 'private' }
ConditionBehavior >> evaluate: expression withContext: aContext [
	^ Smalltalk compiler
		source: expression;
		context: aContext;
		receiver: aContext receiver;
		bindings: { #thisContext -> aContext };
		evaluate
]

{ #category : 'execution' }
ConditionBehavior >> execute [
	self condition ifNil: [ ^true ].
	^(self evaluate: self condition withContext: self context).
]

{ #category : 'initialization' }
ConditionBehavior >> initialize [ 
	"should be executed before once and count etc."
	super initialize.
	self priority: 50.
	self condition: 'false'.
]

{ #category : 'cleanup' }
ConditionBehavior >> remove [ 
	"saving the condition in case the condition behavior is added again later"
	self debugPoint properties at: #Condition put: self condition.
]
