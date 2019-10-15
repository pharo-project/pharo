"
I am a class holding any object. Each time the object changes, I execute a list of commands the user might have register in me.

I am used as an internal mecanism of the ObservableSlot. The slot contains one of my instance and is giving me the blocks to execute when the value change. 

I SHOULD NOT BE USED DIRECTLY BUT VIA AN OBSERVABLE SLOT.
 
In addition, infinite loops of propagation are prevented.

Use case: you have two lists A, and B, and you want to keep their selection synchronised. So when A selection changes, you set B selection. But since B selection changes, you set A selection, and so on… 

This case is prevented by the use of a `lock` variable.
"
Class {
	#name : #ObservableValueHolder,
	#superclass : #Object,
	#instVars : [
		'subscriptions',
		'lock',
		'value'
	],
	#category : #'VariablesLibrary-Observable'
}

{ #category : #'instance creation' }
ObservableValueHolder class >> value: contents [

	^ self new
		rawValue: contents;
		yourself
]

{ #category : #initialization }
ObservableValueHolder >> initialize [
	super initialize.
	lock := false.
	subscriptions := OrderedCollection new
]

{ #category : #printing }
ObservableValueHolder >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPutAll: '[ '; 
		print: self value;
		nextPutAll: ' ]'
]

{ #category : #accessing }
ObservableValueHolder >> rawValue: aValue [
	value := aValue
]

{ #category : #accessing }
ObservableValueHolder >> subscriptions [
	^ subscriptions
]

{ #category : #accessing }
ObservableValueHolder >> subscriptions: anObject [
	subscriptions := anObject
]

{ #category : #transfert }
ObservableValueHolder >> transferSubscriptionsTo: aSpValueHolder [
	aSpValueHolder subscriptions: subscriptions
]

{ #category : #evaluating }
ObservableValueHolder >> value [
	^ value
]

{ #category : #accessing }
ObservableValueHolder >> value: anObject [
	"Handle circular references as explained in the class comment"
	lock ifTrue: [ ^ self ].
	
	lock := true.
	
	[ | oldValue |
	oldValue := value.
	value := anObject.
	self valueChanged: oldValue ]
		ensure: [ lock := false ]
]

{ #category : #accessing }
ObservableValueHolder >> valueChanged [
	self valueChanged: nil
]

{ #category : #accessing }
ObservableValueHolder >> valueChanged: oldValue [
	subscriptions do: [ :block | block cull: self value cull: oldValue ]
]

{ #category : #enumerating }
ObservableValueHolder >> whenChangedDo: aBlock [
	subscriptions add: aBlock
]
