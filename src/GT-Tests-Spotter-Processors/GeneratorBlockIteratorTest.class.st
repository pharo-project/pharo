Class {
	#name : #GeneratorBlockIteratorTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
GeneratorBlockIteratorTest >> testIteratorOnACalculatedCollectionReturnsOnlyFirst [

	| it m |
	 
	it := SptGeneratorBlockIterator on: [ :aBlock | 
		SystemNavigation default allBehaviorsDo: [ :e | e methodsDo: aBlock ] ].	
	
	m := it next.
	
	self assert: m isNotNil.
	self assert: m isCompiledMethod.	
]

{ #category : #tests }
GeneratorBlockIteratorTest >> testIteratorWithEmptyBlockIsAtEnd [

	| it |
	it := SptGeneratorBlockIterator on: [ :aBlock |  ].
	self assert: it atEnd.
]

{ #category : #tests }
GeneratorBlockIteratorTest >> testIteratorWithInfiniteBlockNotCallIfNotUsed [

	| it count |
	count := 0.
	 
	it := SptGeneratorBlockIterator on: [ :aBlock | [aBlock value: count. count := count + 1] repeat ].	
	self assert: count equals: 0.
]

{ #category : #tests }
GeneratorBlockIteratorTest >> testIteratorWithInfiniteBlockOnlyCallFirstTimes [

	| it count |
	count := 1.
	 
	it := SptGeneratorBlockIterator on: [ :aBlock | [aBlock value: count. count := count + 1] repeat ].
	
	it next: 4.
	self assert: count equals: 5.
]

{ #category : #tests }
GeneratorBlockIteratorTest >> testIteratorWithSingleElementIsAtEndAfterNext [

	| it |
	 
	it := SptGeneratorBlockIterator on: [ :aBlock | aBlock value: 1 ].	
	it next.
	
	self assert: it atEnd.
]
