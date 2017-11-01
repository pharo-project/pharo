"
This class stores 32bit Integer points in place. It is used to pass data efficiently to the primitive level during high-bandwidth 2D graphics operations.
"
Class {
	#name : #PointArray,
	#superclass : #IntegerArray,
	#type : #words,
	#traits : 'TPointAccess',
	#classTraits : 'TPointAccess classTrait',
	#category : 'Balloon-Collections'
}

{ #category : #'instance creation' }
PointArray class >> new: n [
	^super new: n*2
]

{ #category : #converting }
PointArray >> asPointArray [
	^ self
]

{ #category : #accessing }
PointArray >> size [
	"Return the number of elements in the receiver"
	^super size // 2
]
