"
I am an implementation of the Stack data structure. My super class is Object and I use a linked list as an instance variable to store the elements. I have a simple API:

- pop
- push:
- top
- isEmpty
- size
- detect:
"
Class {
	#name : 'Stack',
	#superclass : 'Object',
	#instVars : [
		'linkedList'
	],
	#category : 'Collections-Stack-Base',
	#package : 'Collections-Stack',
	#tag : 'Base'
}

{ #category : 'instance creation' }
Stack class >> withAll: aCollection [
	"Answer an instance with same elements as aCollection."

	| newStack |
	newStack := self new.
	aCollection do: [ :e | newStack push: e ].
	^ newStack
]

{ #category : 'comparing' }
Stack >> = anotherStack [

	self == anotherStack ifTrue: [ ^ true ].
	self class == anotherStack class ifFalse: [ ^ false ].
	^ linkedList = anotherStack contents
]

{ #category : 'testing' }
Stack >> detect: detectBlock [

	^ linkedList detect: detectBlock
]

{ #category : 'comparing' }
Stack >> hash [

	^ linkedList hash
]

{ #category : 'testing' }
Stack >> ifEmpty: emptyBlock [

	self isEmpty ifTrue: [ ^ emptyBlock value ]
]

{ #category : 'testing' }
Stack >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [

	^ self isEmpty
		ifTrue: [ emptyBlock value ]
		ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : 'testing' }
Stack >> ifNotEmpty: notEmptyBlock [

	self isEmpty ifFalse: [ ^ notEmptyBlock cull: self ]
]

{ #category : 'initialization' }
Stack >> initialize [

	super initialize.
	linkedList := LinkedList new
]

{ #category : 'testing' }
Stack >> isEmpty [

	^ linkedList isEmpty
]

{ #category : 'testing' }
Stack >> isEmptyOrNil [

	^ self isEmpty
]

{ #category : 'removing' }
Stack >> pop [

	^ linkedList removeFirst
]

{ #category : 'adding' }
Stack >> push: anObject [

	linkedList addFirst: anObject.
	^ anObject
]

{ #category : 'accessing' }
Stack >> size [

	^ linkedList size
]

{ #category : 'accessing' }
Stack >> top [

	^ linkedList first
]
