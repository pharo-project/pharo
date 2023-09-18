Class {
	#name : #TFPoolTest,
	#superclass : #TestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #tests }
TFPoolTest >> testGrowBy [
	| pool |

	pool := TFPool newProvider: [ 1 ] size: 5 releaseBlock: [:x | ].

	5 timesRepeat: [ pool takeOne ].
	self assert: pool size equals: 5.
	pool takeOne.
	self assert: pool size equals: 7
]

{ #category : #tests }
TFPoolTest >> testPopulate [
	| pool |

	pool := TFPool newProvider: [ 1 ] size: 5 releaseBlock: [:x | ].

	self assert: pool elements equals: #(1 1 1 1 1).
	pool growBy: 5.
	self assert: pool elements equals: #(1 1 1 1 1 1 1 1 1 1)
]

{ #category : #tests }
TFPoolTest >> testReturnOne [
	| pool |

	pool := TFPool newProvider: [ 1 ] size: 2 releaseBlock: [:x | ].

	pool takeOne; takeOne.
	self assert: pool isEmpty.
	self assert: pool size equals: 2.
	pool returnOne: 42.
	self deny: pool isEmpty.
	self assert: pool takeOne equals: 42.
	self assert: pool size equals: 2
]

{ #category : #tests }
TFPoolTest >> testTakeOne [
	| pool i |

	i := 0.
	pool := TFPool newProvider: [ i := i + 1 ] size: 2 releaseBlock: [:x | ].

	self assert: pool takeOne equals: 1.
	self assert: pool takeOne equals: 2.
	self assert: pool takeOne equals: 3
]
