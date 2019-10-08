"
SUnit tests for class RunArray
"
Class {
	#name : #RunArrayTest,
	#superclass : #TestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #'tests - instance creation' }
RunArrayTest >> testANewRunArrayIsEmpty [
	self assertEmpty: RunArray new
]

{ #category : #'tests - accessing' }
RunArrayTest >> testAt [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: (array at: 3) equals: 2.
	
	array at: 3 put: 5.
	self assert: (array at: 3) equals: 5

]

{ #category : #'tests - accessing' }
RunArrayTest >> testAtPut [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	
	array at: 3 put: 5.
	self assert: array asArray equals: #(2 2 5 2 2).
	
	array at: 1 put: 1.
	self assert: array asArray equals: #(1 2 5 2 2)
]

{ #category : #'tests - accessing' }
RunArrayTest >> testAtPut2 [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	
	self should: [array at: 0 put: 5] raise: Error.
	self should: [array at: 6 put: 5] raise: Error.
	self should: [array at: $b put: 5] raise: Error
]

{ #category : #'tests - accessing' }
RunArrayTest >> testFirst [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array first equals: 2.
	
	array := 'abcd' as: RunArray.
	self assert: array first equals: $a
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testIsSelfEvaluating [
	| array array2 |
	array := RunArray new: 5 withAll: 2.
	self assert: array isSelfEvaluating.
	array2 := RunArray new: 5 withAll: Object new.
	array2 isSelfEvaluating ifTrue: [self
		assert: (self class compiler evaluate: array2 printString) = array2
		description: 'a self evaluating should evaluate as self']
]

{ #category : #'tests - accessing' }
RunArrayTest >> testLast [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array last equals: 2.
	
	array := 'abcd' as: RunArray.
	self assert: array last equals: $d
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNew [
	
	| array |
	array := RunArray new.
	self assert: array size equals: 0
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewFromArray [
	
	| array |
	array := RunArray newFrom: #($a $b $b $b $b $c $c $a).
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewFromString [
	
	| array |
	array := RunArray newFrom: 'abbbbcca'.
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewWithAll [
	
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array size equals: 5.
	self assert: array asArray equals: #(2 2 2 2 2)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testNewWithSize [
	|array|
	array := RunArray new: 5.
	self assert: array size equals: 5.
	1 to: 5 do: [:index | self assert: (array at: index) isNil]
]

{ #category : #tests }
RunArrayTest >> testRunArrayAdjacentMerge [
	"This demonstrates that adjancent runs with equal attributes are merged."

	| runArray |
	runArray := RunArray new.
	runArray
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5.
	self assert: runArray runs size equals: 2
]

{ #category : #tests }
RunArrayTest >> testRunArrayInvariant [
	"This verifies that the fundamental invariant of a RunArray is always satisfied.
	 This method is a bit tricky. First, it uses Object>>instVarNamed: to access instance 
	 variables for which no accessors are defined. The same method is used by the debuggers 
	 and by various inspectors. The assertion itself explains the meaning of the cached values."
	
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert:
       ((1 to: runArray size) allSatisfy:
           [:idx |  | lastIndex lastOffset lastRun lengthOfPreviousRuns |
               runArray at: idx.  " updates the cached values "
               lastIndex := runArray instVarNamed: 'lastIndex'.
               lastRun := runArray instVarNamed: 'lastRun'.
               lastOffset := runArray instVarNamed: 'lastOffset'.
               lengthOfPreviousRuns 
                   := (1 to: lastRun - 1)
                      inject: 0
                       into: [:sum :idx2 | sum + (runArray runs at: idx2)].
               lastIndex = (lastOffset + lengthOfPreviousRuns + 1) 
           ]
       )
]

{ #category : #tests }
RunArrayTest >> testRunArrayReversal [
  	"This tests the reversal of a RunArray."
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: runArray reversed runs size equals: 3. 
]

{ #category : #tests }
RunArrayTest >> testRunArrayRunsAreNotMerged [
	"This demonstrates that different runs are not merged."
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: runArray runs size equals: 3 
]

{ #category : #tests }
RunArrayTest >> testRunArrayRunsSize [
 	"This demonstrates that the size of a run array is the sum of the sizes of its runs."
	| runArray |
	runArray := RunArray new.
  	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: runArray size equals: 15. 
]

{ #category : #'tests - accessing' }
RunArrayTest >> testRunLengthAt [
	
	| array |
	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	self assert: (array runLengthAt: 1) equals: 1.
	self assert: (array runLengthAt: 2) equals: 2.
	self assert: (array runLengthAt: 3) equals: 1.
	self assert: (array runLengthAt: 4) equals: 3.
	self assert: (array runLengthAt: 5) equals: 2.
	self assert: (array runLengthAt: 6) equals: 1.
	self assert: (array runLengthAt: 7) equals: 2.
	self assert: (array runLengthAt: 8) equals: 1.
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testRunsValues [
	
	| array |
	array := RunArray runs: #(1 4 2 1) values: #($a $b $c $a).
	self assert: array size equals: 8.
	self assert: array asArray equals: #($a $b $b $b $b $c $c $a)
]

{ #category : #'tests - instance creation' }
RunArrayTest >> testScanFromANSICompatibility [
 
	RunArray scanFrom: '()f1dNumber new;;' readStream.
	RunArray scanFrom: '()a1death;;' readStream.
	RunArray scanFrom: '()F1death;;' readStream
]

{ #category : #'tests - accessing' }
RunArrayTest >> testWithStartStopAndValueDo [
	
	| array elements startStops |
	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	elements := OrderedCollection new.
	startStops := OrderedCollection new.
	array withStartStopAndValueDo: [:start :stop :value | elements add: value. startStops add: start->stop].
	
	self assert: elements asArray equals: #($a $b  $c  $d).
	self assert: startStops asArray equals: {1->1 . 2->3 . 4->6 . 7->8}
]
