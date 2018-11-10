"
I'm a weak value holder. 
I keep a weak reference to the instance in contents.
"
Class {
	#name : #IceTipWeakValueHolder,
	#superclass : #Object,
	#type : #weak,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #'instance creation' }
IceTipWeakValueHolder class >> contents: anObject [
	^ self new contents: anObject
]

{ #category : #'instance creation' }
IceTipWeakValueHolder class >> new [
	^ (self basicNew: 1) initialize
]

{ #category : #accessing }
IceTipWeakValueHolder >> contents [
	^ self basicAt: 1
]

{ #category : #accessing }
IceTipWeakValueHolder >> contents: anObject [
	self basicAt: 1 put: anObject
]
