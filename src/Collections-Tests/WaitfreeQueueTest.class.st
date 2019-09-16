Class {
	#name : #WaitfreeQueueTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Atomic'
}

{ #category : #'helper methods' }
WaitfreeQueueTest >> newItems: count [
	"Answer a collection with the requested number of items"

	| collection |

	collection := OrderedCollection new.
	count timesRepeat: [ collection add: AtomicQueueItem new ].
	^collection
]

{ #category : #tests }
WaitfreeQueueTest >> testNextOrNil [

	| queue |

	queue := WaitfreeQueue new.
	1 to: 3 do: [ :i |
		queue 	nextPut: i ].

	"Remove the entries"
	1 to: 3 do: [ :i |
		self assert: queue nextOrNil equals: i ].

	"There should be 0 entries left with no zombies"
	self assert: queue size equals: 0.
	"But a zombie is always left behind"
	self assert: queue itemCount equals: 1.

	"Repeat to check that the number of zombies doesn't grow"
	1 to: 3 do: [ :i |
		queue 	nextPut: i ].

	"Remove the entries"
	1 to: 3 do: [ :i |
		self assert: queue nextOrNil equals: i ].

	"There should be 0 entries left with no zombies"
	self assert: queue size equals: 0.
	"But a zombie is always left behind"
	self assert: queue itemCount equals: 1.

]

{ #category : #tests }
WaitfreeQueueTest >> testNextOrNilSuchThat [

	| queue |

	queue := WaitfreeQueue new.
	1 to: 10 do: [ :i |
		queue nextPut: i ].

	"Remove the first element"
	self assert: (queue nextOrNilSuchThat: [ :o | o = 1 ]) equals: 1.

	"Remove the last element"
	self assert: (queue nextOrNilSuchThat: [ :o | o = 10 ]) equals: 10.

	"Remove the middle elements"
	4 to: 6 do: [ :i |
		self assert: (queue nextOrNilSuchThat: [ :o | o = i ]) equals: i ].

	"There should be 5 elements left with 1 zombie (the last element)"
	self assert: queue size equals: 5.
	"But... all items removed with #nextOrNilSuchThat: leave zombie behind"
	self assert: queue itemCount equals: 6.
	self assert: queue tail isZombie.

	11 to: 13 do: [ :i |
		queue nextPut: i ].

	#(2 3 7 8 9 11 12 13) do: [ :i |
		self assert: queue nextOrNil equals: i ].

	"There should be 0 elements left with one zombie"
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.

	queue nextPut: 11.
	self assert: queue nextOrNil equals: 11.
	self assert: queue nextOrNil equals: nil.

	"There should be 0 elements left with one zombie"
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.

]

{ #category : #tests }
WaitfreeQueueTest >> testZombieFirst [
	"Check that a zombie item at the start of the queue is correctly removed"

	| items queue real |

	items := self newItems: 2.
	queue := WaitfreeQueue withAll: items.
	items first object: queue dummy.
	items last object: #real.

	real := queue nextOrNil.
	self assert: real equals: #real.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.


	items := self newItems: 2.
	queue := WaitfreeQueue withAll: items.
	items first object: queue dummy.
	items last object: #real.

	real := queue nextOrNilSuchThat: [ :v | v = #real ].
	self assert: real equals: #real.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.

]

{ #category : #tests }
WaitfreeQueueTest >> testZombieLast [
	"Check that a zombie item at the end of the queue is correctly left"

	| items queue real |

	items := self newItems: 2.
	queue := WaitfreeQueue withAll: items.
	items first object: #real.
	items last object: queue dummy.

	real := queue nextOrNil.
	self assert: real equals: #real.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.
	self assert: queue nextOrNil isNil.

	items := self newItems: 2.
	queue := WaitfreeQueue withAll: items.
	items first object: #real.
	items last object: queue dummy.

	real := queue nextOrNilSuchThat: [ :v | v = #real ].
	self assert: real equals: #real.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.
	real := queue nextOrNilSuchThat: [ :v | v = #real ].
	self assert: real isNil.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.

]

{ #category : #tests }
WaitfreeQueueTest >> testZombieMiddle [
	"Check that a zombie item at the start of the queue is correctly removed"

	| items queue real |

	items := self newItems: 3.
	queue := WaitfreeQueue withAll: items.
	items first object: #real1.
	items second object: queue dummy.
	items last object: #real2.

	real := queue nextOrNil.
	self assert: real equals: #real1.
	self assert: queue size equals: 1.
	self assert: queue itemCount equals: 2.
	real := queue nextOrNil.
	self assert: real equals: #real2.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.


	items := self newItems: 3.
	queue := WaitfreeQueue withAll: items.
	items first object: #real1.
	items second object: queue dummy.
	items last object: #real2.

	real := queue nextOrNilSuchThat: [ :v | v = #real1 ].
	self assert: real equals: #real1.
	self assert: queue size equals: 1.
	self assert: queue itemCount equals: 2.
	real := queue nextOrNilSuchThat: [ :v | v = #real2 ].
	self assert: real equals: #real2.
	self assert: queue size equals: 0.
	self assert: queue itemCount equals: 1.
	self assert: items last isZombie.

]
