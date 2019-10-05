"
I am a class holding any object inside its unique instance variable. 
Each time the instance variable value changes, an announcement is emitted. 

The instance variable is accessed through `value` and `value:` while the registration is done by `whenChangedDo: aBlock`. 

In addition, infinite loops of propagation are prevented. 
Use case: you have two lists A, and B, and you want to keep their selection synchronised. 
So when A selection changes, you set B selection. 
But since B selection changes, you set A selection, and so on… 

This case is prevented by the use of a `lock` variable.
"
Class {
	#name : #SpValueHolder,
	#superclass : #Object,
	#instVars : [
		'subscriptions',
		'lock',
		'value'
	],
	#category : #'Spec2-ObservableSlot'
}

{ #category : #'instance creation' }
SpValueHolder class >> value: contents [

	^ self new
		rawValue: contents;
		yourself
]

{ #category : #initialization }
SpValueHolder >> initialize [
	super initialize.
	lock := false.
	subscriptions := OrderedCollection new
]

{ #category : #printing }
SpValueHolder >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPutAll: '[ '; 
		print: self value;
		nextPutAll: ' ]'
]

{ #category : #accessing }
SpValueHolder >> rawValue: aValue [
	value := aValue
]

{ #category : #accessing }
SpValueHolder >> subscriptions [
	^ subscriptions
]

{ #category : #accessing }
SpValueHolder >> subscriptions: anObject [
	subscriptions := anObject
]

{ #category : #transfert }
SpValueHolder >> transferSubscriptionsTo: aSpValueHolder [
	aSpValueHolder subscriptions: subscriptions
]

{ #category : #evaluating }
SpValueHolder >> value [
	^ value
]

{ #category : #accessing }
SpValueHolder >> value: anObject [
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
SpValueHolder >> valueChanged [
	self valueChanged: nil
]

{ #category : #accessing }
SpValueHolder >> valueChanged: oldValue [
	subscriptions do: [ :block | block cull: self value cull: oldValue ]
]

{ #category : #enumerating }
SpValueHolder >> whenChangedDo: aBlock [
	subscriptions add: aBlock
]
