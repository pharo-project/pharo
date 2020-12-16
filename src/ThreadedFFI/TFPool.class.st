"
I am a generic pool to be used whith TFFI. 
My main usage will be to keep a pool of external semaphores, avoiding the creation/registration/unregistration of an external semaphore each external call.
"
Class {
	#name : #TFPool,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'elements',
		'provider',
		'initialSize',
		'pointer'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFPool class >> newProvider: aBlock size: aNumber [

	^ self basicNew 
	initializeProvider: aBlock size: aNumber;
	yourself
]

{ #category : #accessing }
TFPool >> elements [

	^ elements copy
]

{ #category : #private }
TFPool >> growBy: aNumber [

	elements := elements, (self populate: (Array new: aNumber)).
]

{ #category : #accessing }
TFPool >> growRate [
 
	^ initialSize // 2
]

{ #category : #initialization }
TFPool >> initialize [

	super initialize.
	mutex := Mutex new
]

{ #category : #initialization }
TFPool >> initializeProvider: aBlock size: aNumber [

	self initialize.
	provider := aBlock.
	initialSize := aNumber.
	pointer := 1.
	elements := self populate: (Array new: initialSize)
]

{ #category : #testing }
TFPool >> isEmpty [
	
	^ elements allSatisfy: #isNil
]

{ #category : #testing }
TFPool >> isOverflown [

	^ pointer > elements size
]

{ #category : #private }
TFPool >> populate: anArray [

	1 to: anArray size do: [ :index |
		anArray at: index put: provider value ].
	
	^ anArray
]

{ #category : #printing }
TFPool >> printOn: stream [

	super printOn: stream.
	stream << '(' << self size printString << ' elements)'
]

{ #category : #accessing }
TFPool >> returnOne: anObject [

	mutex critical: [ 
		| index |
		index := pointer - 1. 
		index > 0 ifFalse: [ 
			"This can happen if a new session starts in middle of a call."
			'Returning an object but pool is full' crTrace. 
			^ self  ]. 
		elements at: index put: anObject.
		pointer := index ]
]

{ #category : #accessing }
TFPool >> size [

	^ elements size
]

{ #category : #accessing }
TFPool >> takeOne [
	| element |

	^ mutex critical: [ 
		| index |
		
		self isOverflown
			ifTrue: [ self growBy: self growRate ].
		
		index := pointer.
		pointer := pointer + 1.
		element := elements at: index.
		elements at: index put: nil.
		element ]
]
