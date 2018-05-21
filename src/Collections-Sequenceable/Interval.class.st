"
I represent a finite arithmetic progression (a range of number).

Description
--------------------------

I allow to iterate easily on a range of number (for example to manupulate an index) with a define step (by default one by one).

Zero step size is not allowed and will raise an error.

I know at which number I begin, at which number I end and the step.

I work with the Number class. I manipulate some numbers and I can be created from a Number.  

Public API and Key Messages
--------------------------

- I implement most of the classic Iterators as #do: or #collect:.

- #from: to: and #from: to: by:  are my two common contructors. But I am usually created by a message send on Number  (See examples).

Examples 
--------------------------

To create an Interval from 1 to 100 there is many ways:

	Interval from: 1 to: 100
	or
	Interval from: 1 to: 100 by: 1
	
	or from a Number 
	
	1 to: 100 
	or 
	1 to: 100 by: 1
	
	You can also use floats or fractions: 
	
	0.1 to: 0.5 by: 0.01
	or
	1/10 to: 1/2 by: 1/100
	
	NB: both expressions will not give exactly the same result. The first will contains only floats and the second only fractions.
	
 
Internal Representation and Key Implementation Points.
--------------------------

    Instance Variables
	start:		<Number> 	The beginning of the Interval.
	step:		<Number> 	The end of the Interval.
	stop:		<Number> 	The step of the interval. If the step is 3 and we begin at 1 the interval will be 1, 4, 7, 10, 13… until the end.

"
Class {
	#name : #Interval,
	#superclass : #SequenceableCollection,
	#instVars : [
		'start',
		'stop',
		'step'
	],
	#category : #'Collections-Sequenceable-Base'
}

{ #category : #'instance creation' }
Interval class >> from: startInteger to: stopInteger [ 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: 1
]

{ #category : #'instance creation' }
Interval class >> from: startInteger to: stopInteger by: stepInteger [ 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of stepNumber."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: stepInteger
]

{ #category : #'instance creation' }
Interval class >> new [
	"Primitive. Create and answer with a new instance of the receiver
	(a class) with no indexable fields. Fail if the class is indexable. Override
	SequenceableCollection new. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 70>
	self isVariable ifTrue: [ ^ self new: 0 ].
	"space must be low"
	OutOfMemory signal.
	^ self new  "retry if user proceeds"

]

{ #category : #'instance creation' }
Interval class >> newFrom: aCollection [ 
	"Answer an instance of me containing the same elements as aCollection."

    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self from: aCollection first to: aCollection last
	by: (aCollection last - aCollection first) // (n - 1).
	aCollection ~= newInterval
		ifTrue: [
			"Give a second chance, because progression might be arithmetic, but = answer false"
			(newInterval hasEqualElements: aCollection) ifFalse: [
				self error: 'The argument is not an arithmetic progression']].
	^newInterval

"	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval    (an error)
	(-4 to: -12 by: -1) as: Interval
	#(2 4 6) asByteArray as: Interval.
"
]

{ #category : #accessing }
Interval class >> streamSpecies [
	^ Array
]

{ #category : #arithmetic }
Interval >> + number [

	^ start + number to: stop + number by: step
]

{ #category : #arithmetic }
Interval >> - number [

	^ start - number to: stop - number by: step
]

{ #category : #comparing }
Interval >> = anObject [

	^ self == anObject
		ifTrue: [true]
		ifFalse: [anObject isInterval
			ifTrue: [start = anObject first
				and: [step = anObject increment
					and: [self last = anObject last]]]
			ifFalse: [super = anObject]]
]

{ #category : #adding }
Interval >> add: newObject [ 
	"Adding to an Interval is not allowed."

	self shouldNotImplement
]

{ #category : #accessing }
Interval >> anyOne [
	"This message will fail for an empty Interval, super would not.
	(2 to: 1) anyOne should fail because empty."

	^self at: 1
]

{ #category : #accessing }
Interval >> at: anInteger [
	"Answer the anInteger'th element."

	(anInteger between: 1 and: self size)
		ifTrue: [ ^ start + (step * (anInteger - 1)) ]
		ifFalse: [ self errorSubscriptBounds: anInteger ]
]

{ #category : #accessing }
Interval >> at: anInteger put: anObject [ 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'
]

{ #category : #enumerating }
Interval >> collect: aBlock [
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do:
		[:i |
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step].
	^ result
]

{ #category : #enumerating }
Interval >> do: aBlock [
	"Evaluate aBlock for each value of the interval.
	Implementation note: instead of repeatedly incrementing the value
		aValue := aValue + step.
	until stop is reached,
	We prefer to recompute value from start
		aValue := start + (index * step).
	This is better for floating points accuracy, while not degrading Integer and Fraction speed too much.
	Moreover, this is consistent with methods #at: and #size"

	| aValue index size |
	index := 0.
	size := self size.
	[index < size]
		whileTrue: [aValue := start + (index * step).
			index := index + 1.
			aBlock value: aValue]
]

{ #category : #accessing }
Interval >> extent [ 
	"Answer the max - min of the receiver interval."
	"(10 to: 50) extent"

	^stop - start
]

{ #category : #accessing }
Interval >> first [ 
	"Refer to the comment in SequenceableCollection|first."

	^start
]

{ #category : #comparing }
Interval >> hash [
	"Hash is reimplemented because = is implemented."

	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: self size
]

{ #category : #accessing }
Interval >> increment [
	"Answer the receiver's interval increment."

	^step
]

{ #category : #accessing }
Interval >> indexOf: anElement startingAt: startIndex ifAbsent: exceptionBlock [
	"startIndex is an positive integer, the collection index where the search is started."
	"during the computation of val , floats are only used when the receiver contains floats"

	| index val |
	(anElement isNumber and:[self rangeIncludes: anElement])
		ifFalse: [^ exceptionBlock value].
	val := anElement - self first / self increment.
	val isFloat
		ifTrue: [(val - val rounded) abs * 100000000 < 1
					ifTrue: [index := val rounded + 1]
					ifFalse: [^ exceptionBlock value]]
		ifFalse: [val isInteger
					ifTrue: [index := val + 1]
					ifFalse: [^ exceptionBlock value]].
	"finally, the value of startIndex comes into play:"
	^ (index between: startIndex and: self size)
		ifTrue: [index]
		ifFalse: [exceptionBlock value]
]

{ #category : #testing }
Interval >> isInterval [

	^ true
]

{ #category : #'self evaluating' }
Interval >> isSelfEvaluating [
	^ self class == Interval
]

{ #category : #accessing }
Interval >> last [ 
	"Refer to the comment in SequenceableCollection|last."

	^stop - (stop - start \\ step)
]

{ #category : #enumerating }
Interval >> permutationsDo: aBlock [
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self asArray permutationsDo: aBlock

]

{ #category : #printing }
Interval >> printOn: aStream [
	aStream nextPut: $(;
	 print: start;
	 nextPutAll: ' to: ';
	 print: stop.
	step ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: step].
	aStream nextPut: $)
]

{ #category : #testing }
Interval >> rangeIncludes: aNumber [
	"Return true if the number lies in the interval between start and stop."

	step >= 0
		ifTrue: [^ aNumber between: start and: stop]
		ifFalse: [^ aNumber between: stop and: start]

]

{ #category : #removing }
Interval >> remove: newObject [ 
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'
]

{ #category : #enumerating }
Interval >> reverseDo: aBlock [ 
	"Evaluate aBlock for each element of my interval, in reverse order.
	Implementation notes: see do: for an explanation on loop detail"

	| aValue index |
	index := self size.
	[index > 0]
		whileTrue: [
			index := index - 1.
			aValue := start + (index * step).
			aBlock value: aValue]
]

{ #category : #private }
Interval >> setFrom: startInteger to: stopInteger by: stepInteger [

	start := startInteger.
	stop := stopInteger.
	step := stepInteger.
	step isZero ifTrue: [ ^ DomainError signal: 'Zero size steps not allowed' ]
]

{ #category : #copying }
Interval >> shuffled [
	"Return an array that contains my elements shuffled in a random order"
	^ self asArray shuffle
]

{ #category : #accessing }
Interval >> size [
	"Answer how many elements the receiver contains."

	step < 0
		ifTrue: [start < stop
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]
		ifFalse: [stop < start
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]
]

{ #category : #sorting }
Interval >> sorted [
	^ self increment >= 0
		ifTrue: [ self copy ]
		ifFalse: [ self last to: self first by: self increment negated ]
]

{ #category : #sorting }
Interval >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison. We convert the interval to an array because intervals can't be changed."
	
	^self asArray sort: aSortBlockOrNil
]

{ #category : #private }
Interval >> species [

	^Array
]

{ #category : #printing }
Interval >> storeOn: aStream [ 
	aStream nextPut: $(;
	 	store: start;
	 	nextPutAll: ' to: ';
	 	store: stop.
	step ~= 1 ifTrue: [aStream nextPutAll: ' by: '; store: step].
	aStream nextPut: $)
]

{ #category : #accessing }
Interval >> sum [
	"Optimized version. Use the sum(n * i - k, i=a..b) = -1/2 * (a - b - 1) * (n * (a + b) - 2 * k) equation with a = 1, n = step, b = self size and k = step - start."

	| b |
	b := self size.
	^b * ((b - 1) * step + (start * 2)) / 2
]
