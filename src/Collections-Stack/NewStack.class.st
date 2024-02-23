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
	#name : 'NewStack',
	#superclass : 'Object',
	#instVars : [
		'linkedList'
	],
	#category : 'Collections-Stack-Base',
	#package : 'Collections-Stack',
	#tag : 'Base'
}

{ #category : 'instance creation' }
NewStack class >> withAll: aCollection [
	"Answer an instance with same elements as aCollection."

	| newStack |
	newStack := self new.
	aCollection do: [ :e | newStack push: e ].
	^ newStack
]

{ #category : 'comparing' }
NewStack >> = anotherStack [

	self == anotherStack ifTrue: [ ^ true ].
	self class == anotherStack class ifFalse: [ ^ false ].
	^ linkedList = anotherStack contents
]

{ #category : 'testing' }
NewStack >> detect: detectBlock [

	^ linkedList detect: detectBlock
]

{ #category : 'comparing' }
NewStack >> hash [

	^ linkedList hash
]

{ #category : 'testing' }
NewStack >> ifEmpty: emptyBlock [

	self isEmpty ifTrue: [ ^ emptyBlock value ]
]

{ #category : 'testing' }
NewStack >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [

	^ self isEmpty
		ifTrue: [ emptyBlock value ]
		ifFalse: [ notEmptyBlock cull: self ]
]

{ #category : 'testing' }
NewStack >> ifNotEmpty: notEmptyBlock [

	self isEmpty ifFalse: [ ^ notEmptyBlock cull: self ]
]

{ #category : 'initialization' }
NewStack >> initialize [

	super initialize.
	linkedList := LinkedList new
]

{ #category : 'testing' }
NewStack >> isEmpty [

	^ linkedList isEmpty
]

{ #category : 'testing' }
NewStack >> isEmptyOrNil [

	^ self isEmpty
]

{ #category : 'removing' }
NewStack >> pop [

	^ linkedList removeFirst
]

{ #category : 'adding' }
NewStack >> push: anObject [

	linkedList addFirst: anObject.
	^ anObject
]

{ #category : 'accessing' }
NewStack >> size [

	^ linkedList size
]

{ #category : 'accessing' }
NewStack >> top [

	^ linkedList first
]
