"
I'm an abstract class grouping the behavior of the code declarations that will be instaled in a behavior, such as methods, class comments, organizations...
"
Class {
	#name : #BehaviorOwnedChunk,
	#superclass : #CodeChunk,
	#instVars : [
		'behaviorName',
		'isMeta'
	],
	#category : #'CodeImport-Chunks'
}

{ #category : #accessing }
BehaviorOwnedChunk >> behaviorName [
	^ behaviorName
]

{ #category : #accessing }
BehaviorOwnedChunk >> behaviorName: aBehaviorName [
	behaviorName := aBehaviorName
]

{ #category : #testing }
BehaviorOwnedChunk >> existsBehavior [
	^self class environment includesKey: behaviorName
]

{ #category : #accessing }
BehaviorOwnedChunk >> isMeta [
	^ isMeta
]

{ #category : #accessing }
BehaviorOwnedChunk >> isMeta: aBoolean [
	isMeta := aBoolean
]

{ #category : #accessing }
BehaviorOwnedChunk >> targetClass [
	| target |
	target := self class environment at: behaviorName.
	^isMeta
		ifTrue: [ target classSide ]
		ifFalse: [ target ]
]
