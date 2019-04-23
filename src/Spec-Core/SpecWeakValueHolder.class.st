"
I'm a weak value holder. 
I keep a weak reference to the instance in contents.
"
Class {
	#name : #SpecWeakValueHolder,
	#superclass : #Object,
	#type : #weak,
	#category : #'Spec-Core-Support'
}

{ #category : #'instance creation' }
SpecWeakValueHolder class >> contents: anObject [
	^ self new contents: anObject
]

{ #category : #'instance creation' }
SpecWeakValueHolder class >> new [
	^ (self basicNew: 1) initialize
]

{ #category : #accessing }
SpecWeakValueHolder >> contents [
	^ self basicAt: 1
]

{ #category : #accessing }
SpecWeakValueHolder >> contents: anObject [
	self basicAt: 1 put: anObject
]
