"
I'm a weak value holder. 
I keep a weak reference to the instance in contents.
"
Class {
	#name : #SpWeakValueHolder,
	#superclass : #Object,
	#type : #weak,
	#category : #'Spec2-Core-Support'
}

{ #category : #'instance creation' }
SpWeakValueHolder class >> contents: anObject [
	^ self new contents: anObject
]

{ #category : #'instance creation' }
SpWeakValueHolder class >> new [
	^ (self basicNew: 1) initialize
]

{ #category : #accessing }
SpWeakValueHolder >> contents [
	^ self basicAt: 1
]

{ #category : #accessing }
SpWeakValueHolder >> contents: anObject [
	self basicAt: 1 put: anObject
]
