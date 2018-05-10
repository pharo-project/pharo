"
I am a collection close to an OrderedCollection at the exception that I keep my elements sorted by using a Block.

Description 
-------------------
Like my superclass, I am a collection that can grow in size but I keep my elements sorted.
Because of that you cannot add elements at a fix index (with #at:put: or #insert:before: methods for example).

The sort block I use should return a boolean. It takes 2 elements and return true if the first parameter should be before the second, else false.

I should be used only if you really need to keep the elements sorted all the time (but there are some exceptions, see at the end). If you do not need it, use OrderedCollection (and possibly his #sort: method).

Public API and Key Messages
-------------------

- class method: #sortBlock:  aBlock 		is a contructor. 
		
- #sort: aBlock 		is a function to change the way I am sorted. I will also update the index of my elements with the new block.

Example
-------------------

	sortColl := SortedCollection sortBlock: [ :elem1 :elem2 | elem1 < elem2 ].
	sortColl
		add: 4;
		add: 2;
		add: 1;
		add: 2.
	sortColl. 		""result: a SortedCollection(1 2 2 4)""
	
	""But you can also add a collection.""
	
	sortColl addAll: #(3 5 0 6).
	sortColl. 		""result: a SortedCollection(0 1 2 2 3 4 5 6)""
			
	""You can charge the block, imagine you have a collection with number and you want at the beginning the odd elements sorted by value then the even elements sorted by value.""
	
	sortColl 
		sort:
			[ :int1 :int2 | 
				((int1 even and: [ int2 even ]) or: [ int1 odd and: [ int2 odd ] ])
					ifTrue: [ int1 < int2 ]
					ifFalse: [ int1 odd ] 
			].
	sortColl 		""result: a SortedCollection(1 3 5 0 2 2 4 6)""
 
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	sortBlock:		<Block> 		This is  a sort block used to keep me sorted. I can take 2 parameters that are two values and I return true if the first parameter should be before the second.

I refuse the methods that add elements at a fix index.

When the user is adding an element, I use some methods like #reSort or #indexForInserting: to add an element at the right position.

Discussion
----------------
(1) sort: and sortBlock: can be used to set an order to my elements but uses different implementation of the sort algorithm... See  https://pharo.fogbugz.com/f/cases/17925/Why-SortedCollection-sort-and-sortBlock-do-not-uses-the-same-method-to-sort.

(2) DO NOT USE ADDLAST:!!!! 
https://pharo.fogbugz.com/f/cases/14812/addLast-should-not-work-in-SortedCollection

x := SortedCollection with: 4 with: 3 with: 2 with: 1 with: 7.
y:=x addLast: 6; yourself.
y isSorted ""-> false""
"
Class {
	#name : #SortedCollection,
	#superclass : #OrderedCollection,
	#instVars : [
		'sortBlock'
	],
	#category : #'Collections-Sequenceable'
}

{ #category : #'instance creation' }
SortedCollection class >> sortBlock: aBlock [ 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^(super new: 10) sortBlock: aBlock
]

{ #category : #comparing }
SortedCollection >> = aSortedCollection [
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	self species = aSortedCollection species ifFalse: [^ false].
	sortBlock = aSortedCollection sortBlock
		ifTrue: [^ super = aSortedCollection]
		ifFalse: [^ false]
]

{ #category : #adding }
SortedCollection >> add: newObject [
	^ super insert: newObject before: (self indexForInserting: newObject)
]

{ #category : #adding }
SortedCollection >> addAll: aCollection [
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection
]

{ #category : #adding }
SortedCollection >> addFirst: newObject [
	self shouldNotImplement
]

{ #category : #accessing }
SortedCollection >> at: anInteger put: anObject [
	self shouldNotImplement
]

{ #category : #enumerating }
SortedCollection >> collect: aBlock [ 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into an OrderedCollection. Answer the new collection.
	We cannot assume that the result is sorted, because aBlock can transform the 
	elements in arbitrary ways.  Thus, we must override the superclass in order
	to produce an OrderedCollection instead of a SortedCollection."

	| newCollection | 
	newCollection := OrderedCollection new: self size.
	self do: [:each | newCollection addLast: (aBlock value: each)].
	^ newCollection
]

{ #category : #adding }
SortedCollection >> copyEmpty [
	"Answer a copy of the receiver without any of the receiver's elements."

	^self species sortBlock: sortBlock
]

{ #category : #private }
SortedCollection >> defaultSort: i to: j [ 
	"Sort elements i through j of self to be nondescending according to
	sortBlock."	"Assume the default sort block ([:x :y | x <= y])."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n := j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di := array at: i.
	dj := array at: j.
	(di <= dj) "i.e., should di precede dj?"
		ifFalse: 
			[array swap: i with: j.
			 tt := di.
			 di := dj.
			 dj := tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij := (i + j) // 2.  "ij is the midpoint of i and j."
			 dij := array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (di <= dij) "i.e. should di precede dij?"
			   ifTrue: 
				[(dij <= dj) "i.e., should dij precede dj?"
				  ifFalse: 
					[array swap: j with: ij.
					 dij := dj]]
			   ifFalse:  "i.e. di should come after dij"
				[array swap: i with: ij.
				 dij := di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k := i.
				 l := j.
				 [[l := l - 1.  k <= l and: [dij <= (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k := k + 1.  k <= l and: [(array at: k) <= dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self defaultSort: i to: l.
				self defaultSort: k to: j]]
]

{ #category : #enumerating }
SortedCollection >> flatCollect: aBlock [ 
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"
	
	^ self flatCollect: aBlock as: OrderedCollection
]

{ #category : #private }
SortedCollection >> indexForInserting: newObject [

	| index low high |
	low := firstIndex.
	high := lastIndex.
	sortBlock 
		ifNil: [[index := high + low // 2.  low > high]
			whileFalse: 
				[((array at: index) <= newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]]
		ifNotNil: [[index := high + low // 2.  low > high]
			whileFalse: 
				[(sortBlock value: (array at: index) value: newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]].
	^low
]

{ #category : #private }
SortedCollection >> insert: anObject before: spot [
	self shouldNotImplement
]

{ #category : #splitjoin }
SortedCollection >> join: aCollection [ 
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"Curiously addAllLast: does not trigger a reSort, so we must do it here."
	^ (super join: aCollection) reSort; yourself

]

{ #category : #accessing }
SortedCollection >> median [
	"Return the middle element, or as close as we can get."

	^ self at: self size + 1 // 2
]

{ #category : #private }
SortedCollection >> reSort [
	self sort: firstIndex to: lastIndex
]

{ #category : #accessing }
SortedCollection >> sort: aSortBlock [ 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."
 
 	super sort: aSortBlock.
 	sortBlock := aSortBlock
]

{ #category : #private }
SortedCollection >> sort: i to: j [ 
	"Sort elements i through j of self to be nondescending according to
	sortBlock."

	| di dij dj tt ij k l n |
	sortBlock ifNil: [^self defaultSort: i to: j].
	"The prefix d means the data at that index."
	(n := j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di := array at: i.
	dj := array at: j.
	(sortBlock value: di value: dj) "i.e., should di precede dj?"
		ifFalse: 
			[array swap: i with: j.
			 tt := di.
			 di := dj.
			 dj := tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij := (i + j) // 2.  "ij is the midpoint of i and j."
			 dij := array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (sortBlock value: di value: dij) "i.e. should di precede dij?"
			   ifTrue: 
				[(sortBlock value: dij value: dj) "i.e., should dij precede dj?"
				  ifFalse: 
					[array swap: j with: ij.
					 dij := dj]]
			   ifFalse:  "i.e. di should come after dij"
				[array swap: i with: ij.
				 dij := di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k := i.
				 l := j.
				 [[l := l - 1.  k <= l and: [sortBlock value: dij value: (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k := k + 1.  k <= l and: [sortBlock value: (array at: k) value: dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self sort: i to: l.
				self sort: k to: j]]
]

{ #category : #accessing }
SortedCollection >> sortBlock [
	"Answer the blockContext which is the criterion for sorting elements of 
	the receiver."

	^sortBlock
]

{ #category : #accessing }
SortedCollection >> sortBlock: aBlock [ 
	"Make the argument, aBlock, be the criterion for ordering elements of the 
	receiver."

	sortBlock := aBlock.
	"sortBlocks with side effects may not work right"
	self size > 0 ifTrue: [self reSort]
]
