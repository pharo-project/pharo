Extension { #name : #DoubleLinkedList }

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> head [

	^ head
]

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> head: aDoubleLink [

	head := aDoubleLink
]

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> noneSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver. If aBlock returns false for all elements return true. Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
]

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> removeCycledLink: link [
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

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> tail [

	^ tail
]

{ #category : #'*GT-Spotter' }
DoubleLinkedList >> tail: aDoubleLink [

	tail := aDoubleLink
]
