"
I model a node of the call tree which is not the root.

Additionally to my superclass, I store:
- #astNode : The node of the method's AST corresponding to the call I model.
- #hasBeenExecuted : A Boolean allowing to check whether I have been executed or not. This Boolean value is updated by the #metaLink.
- #metaLink : The metaLink which will watch whether I am executed or not and set #hasBeenExecuted accordingly.
"
Class {
	#name : 'RTFSelfCallNode',
	#superclass : 'RTFSelfCallRootNode',
	#instVars : [
		'astNode',
		'hasBeenExecuted',
		'metaLink'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'instance creation' }
RTFSelfCallNode class >> astNode: aMessageNode compiledMethod: aCompiledMethod [
	^ self new
		astNode: aMessageNode;
		compiledMethod: aCompiledMethod;
		yourself
]

{ #category : 'visiting' }
RTFSelfCallNode >> acceptVisitor: aRTFSelfCallVisitor [
	^ aRTFSelfCallVisitor visitRTFSelfCallNode: self
]

{ #category : 'accessing' }
RTFSelfCallNode >> astNode [
	^ astNode
]

{ #category : 'accessing' }
RTFSelfCallNode >> astNode: anObject [
	astNode := anObject
]

{ #category : 'accessing' }
RTFSelfCallNode >> hasBeenExecuted [
	^ hasBeenExecuted
]

{ #category : 'accessing' }
RTFSelfCallNode >> hasBeenExecuted: anObject [
	hasBeenExecuted := anObject
]

{ #category : 'initialization' }
RTFSelfCallNode >> initialize [
	super initialize.
	self hasBeenExecuted: false
]

{ #category : 'marking' }
RTFSelfCallNode >> markAsExecuted [
	self hasBeenExecuted: true
]

{ #category : 'accessing' }
RTFSelfCallNode >> metaLink [
	^ metaLink
]

{ #category : 'accessing' }
RTFSelfCallNode >> metaLink: anObject [
	metaLink := anObject
]
