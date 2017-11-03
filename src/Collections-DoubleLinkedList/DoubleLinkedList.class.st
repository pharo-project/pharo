"
I am DoubleLinkedList, an ordered list data structure consisting of objects, most likely DoubleLinks or something compatible, connected to each other by forward and backwards links.

Note that some of my API deals with the elements that I hold, like any other collection, while some of my API references the links that I use internally (those usually have the word link in the selector name). Some methods accepts both values or links as argument (like #add:). Because I expose some if my internal structure, I can be broken quite easily.


"
Class {
	#name : #DoubleLinkedList,
	#superclass : #Object,
	#instVars : [
		'head',
		'tail'
	],
	#category : #Collections-DoubleLinkedList
}

{ #category : #adding }
DoubleLinkedList >> add: anObjectOrLink [
	"Add anObjectOrLink at the end of me.
	Return the internal link object."
	
	^ self addLast: anObjectOrLink
]

{ #category : #adding }
DoubleLinkedList >> add: anObjectOrLink afterLink: otherLink [
	"Add anObjectOrLink right after otherLink in me.
	When otherLink is not part of me, the result is undefined.
	Return the internal link object."

	| link otherLinkSuccessor |
	otherLink = tail
		ifTrue: [ ^ self addLast: anObjectOrLink ].
	link := anObjectOrLink asDoubleLink.
	otherLinkSuccessor := otherLink nextLink.
	otherLink nextLink: link.
	link previousLink: otherLink.
	link nextLink: otherLinkSuccessor.
	otherLinkSuccessor previousLink: link.
	^ link
]

{ #category : #adding }
DoubleLinkedList >> add: anObjectOrLink beforeLink: otherLink [
	"Add anObjectOrLink right before otherLink in me.
	When otherLink is not part of me, the result is undefined.
	Return the internal link object."

	| link otherLinkPredeccessor |
	otherLink = head
		ifTrue: [ ^ self addFirst: anObjectOrLink ].
	link := anObjectOrLink asDoubleLink.
	otherLinkPredeccessor := otherLink previousLink.
	otherLink previousLink: link.
	link nextLink: otherLink.
	link previousLink: otherLinkPredeccessor.
	otherLinkPredeccessor nextLink: link.
	^ link
]

{ #category : #adding }
DoubleLinkedList >> addAll: collection [
	"Add all elements of collection to me, in order.
	Each can be either a value object or a link."
	
	collection do: [ :each | self add: each ]
]

{ #category : #adding }
DoubleLinkedList >> addFirst: anObjectOrLink [
	"Add anObjectOrLink to me, so that it becomes the first one.
	Return the internal link object."
	
	| link |
	link := anObjectOrLink asDoubleLink.
	link nextLink: head.
	head ifNotNil: [ head previousLink: link ].
	tail ifNil: [ tail := link ].
	head := link.
	^ link
]

{ #category : #adding }
DoubleLinkedList >> addLast: anObjectOrLink [
	"Add anObjectOrLink to me, so that it becomes the last one.
	Return the internal link object."

	| link |
	link := anObjectOrLink asDoubleLink.
	link previousLink: tail.
	tail ifNotNil: [ tail nextLink: link ].
	head ifNil: [ head := link ].
	tail := link.
	^ link
]

{ #category : #converting }
DoubleLinkedList >> asArray [
	"Return a new Array containing my elements"
	
	^ Array streamContents: [ :out |
		self do: [ :each | out nextPut: each ] ]
]

{ #category : #enumerating }
DoubleLinkedList >> collect: block [
	"Execute block for each of my elements and return a new instance of me containing the results."
	
	| result |
	result := self class new.
	self do: [ :each |
		result add: (block value: each) ].
	^ result
]

{ #category : #enumerating }
DoubleLinkedList >> do: block [
	"Execute block for each of my elements."

	self linksDo: [ :each | block value: each value ]
]

{ #category : #private }
DoubleLinkedList >> emptyCheck [
	self isEmpty
		ifTrue: [ CollectionIsEmpty signalWith: self ]
]

{ #category : #accessing }
DoubleLinkedList >> first [
	"Returnt the first element that I hold, also known as my head value.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ head value
]

{ #category : #accessing }
DoubleLinkedList >> firstLink [
	"Returnt the first link that I hold, also known as my head.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ head
]

{ #category : #testing }
DoubleLinkedList >> includes: object [
	"Return true when I hold object as a value, false otherwise."
	
	self do: [ :each |
		each = object ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
DoubleLinkedList >> isEmpty [
	"Return true when I contain no elements, false otherwise."

	^ head isNil and: [ tail isNil ]
]

{ #category : #accessing }
DoubleLinkedList >> last [
	"Returnt the last element that I hold, also known as my tail value.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ tail value
]

{ #category : #accessing }
DoubleLinkedList >> lastLink [
	"Returnt the last link that I hold, also known as my tail.
	Signal a CollectionIsEmpty excpetion when I am empty."
	
	self emptyCheck.
	^ tail
]

{ #category : #enumerating }
DoubleLinkedList >> linksDo: block [
	"Execute block for each of the links that I hold internally."
	
	| current |
	current := head.
	[ current isNil ] 
		whileFalse: [ 
	 		block value: current.
			current := current nextLink ]
]

{ #category : #enumerating }
DoubleLinkedList >> reject: block [
	"Execute block for each of my elements and return a new instance of me 
	containing those elements for which the block returned false."

	| result |
	result := self class new.
	self do: [ :each |
		(block value: each)
			ifFalse: [ result add: each ] ].
	^ result
]

{ #category : #removing }
DoubleLinkedList >> removeAll [
	"Remove all the elements that I hold."
	
	head := tail := nil
]

{ #category : #removing }
DoubleLinkedList >> removeFirst [
	"Remove my first element.
	Signal a CollectionIsEmpty excpetion when I am empty.
	Return the removed internal link."
	
	| link |
	self emptyCheck.
	link := head.
	head := head nextLink.
	head 
		ifNil: [ tail := nil ]
		ifNotNil: [ head previousLink: nil ].
	link clearLinks.
	^ link
]

{ #category : #removing }
DoubleLinkedList >> removeLast [
	"Remove my last element.
	Signal a CollectionIsEmpty excpetion when I am empty.
	Return the removed internal link."

	| link |
	self emptyCheck.
	link := tail.
	tail := tail previousLink.
	tail 
		ifNil: [ head := nil ]
		ifNotNil: [ tail nextLink: nil ].
	link clearLinks.
	^ link
]

{ #category : #removing }
DoubleLinkedList >> removeLink: link [
	"Remove the specified link.
	When otherLink is not part of me, the result is undefined.
	Return the removed internal link."

	| predecessor successor |
	predecessor := link previousLink.
	successor := link nextLink.
	predecessor
		ifNil: [ head := successor ]
		ifNotNil: [ predecessor nextLink: successor ].
	successor
		ifNil: [ tail := predecessor ]
		ifNotNil: [ successor previousLink: predecessor ].
	link clearLinks.
	^ link
]

{ #category : #enumerating }
DoubleLinkedList >> reverseDo: block [
	"Execute block for each of my elements, in reverse order."

	self reverseLinksDo: [ :each | block value: each value ]
]

{ #category : #enumerating }
DoubleLinkedList >> reverseLinksDo: block [
	"Execute block for each of the links that I hold internally, in reverse order."

	| current |
	current := tail.
	[ current isNil ] 
		whileFalse: [ 
	 		block value: current.
			current := current previousLink ]
]

{ #category : #enumerating }
DoubleLinkedList >> select: block [
	"Execute block for each of my elements and return a new instance of me 
	containing those elements for which the block returned true."

	| result |
	result := self class new.
	self do: [ :each |
		(block value: each)
			ifTrue: [ result add: each ] ].
	^ result
]
