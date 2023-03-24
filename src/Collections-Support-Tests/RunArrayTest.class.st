"
SUnit tests for class RunArray
"
Class {
	#name : #RunArrayTest,
	#superclass : #TestCase,
	#traits : 'TSequenceableTruncatedToSize',
	#classTraits : 'TSequenceableTruncatedToSize classTrait',
	#instVars : [
		'collectionWith5Elements',
		'array'
	],
	#category : #'Collections-Support-Tests'
}

{ #category : #requirements }
RunArrayTest >> collectionWith5Elements [

	^ collectionWith5Elements
]

{ #category : #running }
RunArrayTest >> setUp [

	super setUp.
	collectionWith5Elements := RunArray newFrom: #(10 20 30 40 50)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testANewRunArrayIsEmpty [
	self assertEmpty: RunArray new
]

{ #category : #'tests - adding' }
RunArrayTest >> testAdd [

	array := RunArray new.
	self assertEmpty: array.

	array add: $A.
	self assert: array size equals: 1.
	array add: $A.
	self assert: array size equals: 2.
	array add: $B.
	self assert: array size equals: 3
]

{ #category : #'tests - adding' }
RunArrayTest >> testAddAddsAsLastElement [

	array := RunArray new.
	self assertEmpty: array.

	array add: $A.
	array add: $A.
	array add: $B.
	self assert: (array at: 3) equals: $B
]

{ #category : #'tests - accessing' }
RunArrayTest >> testAt [

	array := RunArray new: 5 withAll: 2.
	self assert: (array at: 3) equals: 2.

	array at: 3 put: 5.
	self assert: (array at: 3) equals: 5
]

{ #category : #'tests - accessing' }
RunArrayTest >> testAtPut [

	array := RunArray new: 5 withAll: 2.

	array at: 3 put: 5.
	self assert: array asArray equals: #(2 2 5 2 2).

	array at: 1 put: 1.
	self assert: array asArray equals: #(1 2 5 2 2)
]

{ #category : #'tests - accessing' }
RunArrayTest >> testAtPut2 [

	array := RunArray new: 5 withAll: 2.

	self should: [array at: 0 put: 5] raise: Error.
	self should: [array at: 6 put: 5] raise: Error.
	self should: [array at: $b put: 5] raise: Error
]

{ #category : #tests }
RunArrayTest >> testFindLast [

	self assert: ((RunArray newFrom: #(10 20 30 40 50)) findLast: [ :each | each > 10 ]) equals: 5.
	self assert: ((RunArray newFrom: #(10 20 30 40 50)) findLast: [ :each | each > 60 ]) equals: 0
]

{ #category : #'tests - accessing' }
RunArrayTest >> testFirst [

	array := RunArray new: 5 withAll: 2.
	self assert: array first equals: 2.

	array := 'abcd' as: RunArray.
	self assert: array first equals: $a
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testIsSelfEvaluating [
	| array2 |
	array := RunArray new: 5 withAll: 2.
	self assert: array isSelfEvaluating.
	array2 := RunArray new: 5 withAll: Object new.
	array2 isSelfEvaluating ifTrue: [self
		assert: (self class compiler evaluate: array2 printString) = array2
		description: 'a self evaluating should evaluate as self']
]

{ #category : #'tests - accessing' }
RunArrayTest >> testLast [

	array := RunArray new: 5 withAll: 2.
	self assert: array last equals: 2.

	array := 'abcd' as: RunArray.
	self assert: array last equals: $d
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNew [

	array := RunArray new.
	self assertEmpty: array
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewFromArray [

	array := RunArray newFrom: #($a $b $b $b $b $c $c $a).
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewFromString [

	array := RunArray newFrom: 'abbbbcca'.
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewWithAll [

	array := RunArray new: 5 withAll: 2.
	self assert: array size equals: 5.
	self assert: array asArray equals: #(2 2 2 2 2)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewWithSize [

	array := RunArray new: 5.
	self assert: array size equals: 5.
	1 to: 5 do: [:index | self assert: (array at: index) isNil]
]

{ #category : #tests }
RunArrayTest >> testRunArrayAdjacentMerge [
	"This demonstrates that adjancent runs with equal attributes are merged."

	array := RunArray new.
	array
		add: $a withOccurrences: 5;
		add: $b withOccurrences: 5;
		add: $b withOccurrences: 5.
	self assert: array runs size equals: 2
]

{ #category : #tests }
RunArrayTest >> testRunArrayInvariant [
	"This verifies that the fundamental invariant of a RunArray is always satisfied.
	 This method is a bit tricky. First, it uses Object>>instVarNamed: to access instance
	 variables for which no accessors are defined. The same method is used by the debuggers
	 and by various inspectors. The assertion itself explains the meaning of the cached values."

	array := RunArray new.
	array
		add: $a withOccurrences: 5;
		add: $b withOccurrences: 5;
		add: $a withOccurrences: 5.
	self assert: ((1 to: array size) allSatisfy: [ :idx |
			 | lastIndex lastOffset lastRun lengthOfPreviousRuns |
			 array at: idx. " updates the cached values "
			 lastIndex := array instVarNamed: 'lastIndex'.
			 lastRun := array instVarNamed: 'lastRun'.
			 lastOffset := array instVarNamed: 'lastOffset'.
			 lengthOfPreviousRuns := (1 to: lastRun - 1) inject: 0 into: [ :sum :idx2 | sum + (array runs at: idx2) ].
			 lastIndex = (lastOffset + lengthOfPreviousRuns + 1) ])
]

{ #category : #tests }
RunArrayTest >> testRunArrayReversal [
	"This tests the reversal of a RunArray."

	array := RunArray new.
	array
		add: $a withOccurrences: 5;
		add: $b withOccurrences: 5;
		add: $a withOccurrences: 5.
	self assert: array reversed runs size equals: 3
]

{ #category : #tests }
RunArrayTest >> testRunArrayRunsAreNotMerged [
	"This demonstrates that different runs are not merged."

	array := RunArray new.
	array
		add: $a withOccurrences: 5;
		add: $b withOccurrences: 5;
		add: $a withOccurrences: 5.
	self assert: array runs size equals: 3
]

{ #category : #tests }
RunArrayTest >> testRunArrayRunsSize [
	"This demonstrates that the size of a run array is the sum of the sizes of its runs."

	array := RunArray new.
	array
		add: $a withOccurrences: 5;
		add: $b withOccurrences: 5;
		add: $a withOccurrences: 5.
	self assert: array size equals: 15
]

{ #category : #'tests - accessing' }
RunArrayTest >> testRunLengthAt [

	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	self assert: (array runLengthAt: 1) equals: 1.
	self assert: (array runLengthAt: 2) equals: 2.
	self assert: (array runLengthAt: 3) equals: 1.
	self assert: (array runLengthAt: 4) equals: 3.
	self assert: (array runLengthAt: 5) equals: 2.
	self assert: (array runLengthAt: 6) equals: 1.
	self assert: (array runLengthAt: 7) equals: 2.
	self assert: (array runLengthAt: 8) equals: 1
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testRunsValues [

	array := RunArray runs: #(1 4 2 1) values: #($a $b $c $a).
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - accessing' }
RunArrayTest >> testWithStartStopAndValueDo [

	| elements startStops |
	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	elements := OrderedCollection new.
	startStops := OrderedCollection new.
	array withStartStopAndValueDo: [:start :stop :value | elements add: value. startStops add: start->stop].

	self assert: elements asArray equals: #($a $b  $c  $d).
	self assert: startStops asArray equals: {1->1 . 2->3 . 4->6 . 7->8}
]
