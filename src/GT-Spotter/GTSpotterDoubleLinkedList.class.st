Class {
	#name : #GTSpotterDoubleLinkedList,
	#superclass : #DoubleLinkedList,
	#category : #'GT-Spotter-Utils'
}

{ #category : #accessing }
GTSpotterDoubleLinkedList >> head [

	^ head
]

{ #category : #accessing }
GTSpotterDoubleLinkedList >> head: aDoubleLink [

	head := aDoubleLink
]

{ #category : #enumerating }
GTSpotterDoubleLinkedList >> linksDo: aBlock [
	| current |
	current := head.
	[ current isNil ] 
		whileFalse: [ 
	 		aBlock value: current.
			current := current nextLink ]
]

{ #category : #'as yet unclassified' }
GTSpotterDoubleLinkedList >> removeCycledLink: link [
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
	
	"I'm the only one left, clear linked list and return"
	(link = head & (link = tail)) ifTrue: [ head := nil. tail := nil. ^ link ].
	
	link = head ifTrue: [ head := successor ].
	link = tail ifTrue: [ tail := predecessor ].
	
	^ link
]

{ #category : #accessing }
GTSpotterDoubleLinkedList >> tail [

	^ tail
]

{ #category : #accessing }
GTSpotterDoubleLinkedList >> tail: aDoubleLink [

	tail := aDoubleLink
]
