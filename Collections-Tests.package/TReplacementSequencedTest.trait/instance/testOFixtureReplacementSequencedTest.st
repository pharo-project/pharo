tests - fixture
testOFixtureReplacementSequencedTest

	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	
	self elementInForReplacement.
	self assert: (self nonEmpty includes: self elementInForReplacement ) .
	
	self newElement.
	
	self firstIndex.
	self assert: (self firstIndex >= 1 & self firstIndex <= self nonEmpty size).
	
	self secondIndex.
	self assert: (self secondIndex >= 1 & self secondIndex <= self nonEmpty size).
	
	self assert: self firstIndex <=self secondIndex .
	
	self replacementCollection.
	
	self replacementCollectionSameSize.
	self assert: (self secondIndex  - self firstIndex +1)= self replacementCollectionSameSize size
	