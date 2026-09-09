"
Examples that wait on a semaphore

Two cases:

- we add the metalink before the pc that the waiting method is executing
- ae add it *after*.

The after case is simpler to implement as we do not need to change the PC of the method on the stack

PLAN:

solve second case first.
"
Class {
	#name : 'ReflectivityExampleOnStack',
	#superclass : 'Object',
	#instVars : [
		'tag',
		'semaphore'
	],
	#category : 'Reflectivity-Tests-Data',
	#package : 'Reflectivity-Tests',
	#tag : 'Data'
}

{ #category : 'waiting' }
ReflectivityExampleOnStack >> continue [

	semaphore signal
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphore [

	2 + 3.
	self wait.
	7 + 5.
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreDirectly [

	2 + 3.
	semaphore wait.
	7 + 5
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreInBlock [

	| a block |
	a := 2.
	block := [
	         a := 2 + 3.
	         semaphore wait.
	         a := 7 + 5 ].
	^ a + block value
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreInEmbeddedBlock [ 

	| a block | 
	a := 2.
	block := [
	         | b block2 |
	         a := 2 + 3.
	         block2 := [
	                   b := 2 + 3.
	                   semaphore wait.
	                   b := 7 + 5 ].
	         a := 4 + 2.
	         block2 value ].
	^ a + block value
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreInInlineBlock [

	2 + 3.
	true ifTrue: [
		1 + 1.
		semaphore wait. 
		3 + 3 ].
	7 + 5
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreInMethodWithOuterBlockStoreInVariableBefore [

	| a block |
	a := 2.
	block := [
	         | b block2 |
	         a := 2 + 3.
	         block2 := [
	                   b := 2 + 3.
	                   b := 4 + 2.
	                   b := 7 + 5 ].
	         a := 4 + 2.
	         block2 value ].
	semaphore wait.
	^ a + block value
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreInOuterBlockWithEmbeddedBlockStoreInVariableBefore [

	| a block |
	a := 2.
	block := [
	         | b block2 |
	         a := 2 + 3.
	         block2 := [
	                   b := 2 + 3.
	                   b := 4 + 2.
	                   b := 7 + 5 ].
	         semaphore wait.
	         a := 4 + 2.
	         block2 value ].
	^ a + block value
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreWithBlockAfter [

	| a |
	a := 2.
	semaphore wait.

	^ a + [
	  a := 2 + 3.
	  a := 4 + 2.
	  a := 7 + 5 ] value
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> exampleMethodWaitingOnSemaphoreWithBlockStoreInVariableBefore [

	| a block |
	a := 2.
	block := [
	         a := 2 + 3.
	         a := 4 + 2.
	         a := 7 + 5 ].
	semaphore wait.
	^ a + block value
]

{ #category : 'initialization' }
ReflectivityExampleOnStack >> initialize [
	semaphore := Semaphore new.
]

{ #category : 'accessing' }
ReflectivityExampleOnStack >> tag [
	^tag
]

{ #category : 'accessing' }
ReflectivityExampleOnStack >> tagExec: anObject [
	tag := anObject
]

{ #category : 'waiting' }
ReflectivityExampleOnStack >> wait [
	semaphore wait.
]

{ #category : 'examples' }
ReflectivityExampleOnStack >> wait2 [

	self wait
]
