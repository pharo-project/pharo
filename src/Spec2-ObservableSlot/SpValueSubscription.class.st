"
I'm a value holder subscription. 
I keep an action and I can be able to execute it.

My purpose is to give entity to the change action, so users can operate over the actions (for example unsubscribing them)
"
Class {
	#name : #SpValueSubscription,
	#superclass : #Object,
	#instVars : [
		'action'
	],
	#category : #'Spec2-ObservableSlot'
}

{ #category : #'instance creation' }
SpValueSubscription class >> with: aBlock [

	^ self new 
		action: aBlock;
		yourself
]

{ #category : #accessing }
SpValueSubscription >> action [

	^ action
]

{ #category : #accessing }
SpValueSubscription >> action: aBlock [

	action := aBlock
]

{ #category : #evaluating }
SpValueSubscription >> cull: newValue cull: oldValue [

	action 
		cull: newValue 
		cull: oldValue
]
