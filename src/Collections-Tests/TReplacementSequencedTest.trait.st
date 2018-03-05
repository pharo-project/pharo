"
A trait for test purposes
"
Trait {
	#name : #TReplacementSequencedTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TReplacementSequencedTest >> elementInForReplacement [
" return an element included in 'nonEmpty' "
^ self nonEmpty anyOne.
]

{ #category : #requirements }
TReplacementSequencedTest >> firstIndex [
" return an index between 'nonEmpty' bounds that is < to 'second index' "
	^self explicitRequirement
]

{ #category : #requirements }
TReplacementSequencedTest >> newElement [
"return an element that will be put in the collection in place of another"
	self explicitRequirement
]

{ #category : #requirements }
TReplacementSequencedTest >> nonEmpty [
	^self explicitRequirement
]

{ #category : #requirements }
TReplacementSequencedTest >> replacementCollection [
" return a collection that will be put into 'nonEmpty' "
	^ self replacementCollectionSameSize, self 	replacementCollectionSameSize   
]

{ #category : #requirements }
TReplacementSequencedTest >> replacementCollectionSameSize [
" return a collection of size (secondIndex - firstIndex + 1)"
	^self explicitRequirement
]

{ #category : #requirements }
TReplacementSequencedTest >> secondIndex [
" return an index between 'nonEmpty' bounds that is > to 'first index' "
	^self explicitRequirement
]

{ #category : #'tests - fixture' }
TReplacementSequencedTest >> testOFixtureReplacementSequencedTest [

	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	
	self elementInForReplacement.
	self assert: (self nonEmpty includes: self elementInForReplacement).
	
	self newElement.
	
	self firstIndex.
	self assert: (self firstIndex >= 1 & self firstIndex <= self nonEmpty size).
	
	self secondIndex.
	self assert: (self secondIndex >= 1 & self secondIndex <= self nonEmpty size).
	
	self assert: self firstIndex <= self secondIndex.
	
	self replacementCollection.
	
	self replacementCollectionSameSize.
	self assert: (self secondIndex  - self firstIndex +1) = self replacementCollectionSameSize size
	
]

{ #category : #'tests - replacing' }
TReplacementSequencedTest >> testReplaceAllWith [
	| result collection oldElement newElement oldOccurrences |
	collection := self nonEmpty.
	result := collection copy.
	oldElement := self elementInForReplacement.
	newElement := self newElement.
	oldOccurrences := (result occurrencesOf: oldElement) + (result occurrencesOf: newElement).
	result replaceAll: oldElement with: newElement.
	
	self assert: oldOccurrences = (result occurrencesOf: newElement)
]

{ #category : #'tests - replacing' }
TReplacementSequencedTest >> testReplaceFromToWith [
	| result collection replacementCollec firstInd secondInd |
	collection := self nonEmpty .
	replacementCollec := self replacementCollectionSameSize.
	firstInd := self firstIndex.
	secondInd := self secondIndex.
	result := collection copy.
	result replaceFrom: firstInd to: secondInd  with: replacementCollec.
	
	"verify content of 'result' : "
	"first part of 'result'' : '"
	
	1 to: (firstInd - 1) do: [:i | self assert: (collection at:i) = (result at: i) ].
	
	" middle part containing replacementCollection : "
	
	firstInd to: (firstInd + replacementCollec size - 1) do: 
		[:i | self assert: (result at: i) = (replacementCollec at: (i - firstInd + 1)) ].
	
	" end part :"
	(firstInd + replacementCollec size) to: (result size) do:
		[:i|
		self assert: (result at: i) = (collection at: (secondInd + 1 - (firstInd + replacementCollec size) + i)) ]
]

{ #category : #'tests - replacing' }
TReplacementSequencedTest >> testReplaceFromToWithStartingAt [
	| result repStart collection replacementCollec firstInd secondInd |
	collection := self nonEmpty.
	result := collection copy.
	replacementCollec := self replacementCollectionSameSize.
	firstInd := self firstIndex.
	secondInd := self secondIndex.
	repStart := replacementCollec size - (secondInd - firstInd + 1) + 1.
	result replaceFrom: firstInd to: secondInd with: replacementCollec startingAt: repStart.
	
	"verify content of 'result'"
	"first part of 'result'' : '" 	
	1 to: (firstInd - 1) do: [:i | self assert: (collection at:i) equals: (result at: i) ].
	
	"middle part containing replacementCollection"
	
	firstInd to: (replacementCollec size - repStart +1) do: 
		[:i| self assert: (result at: i) equals: (replacementCollec at: (repStart + (i - firstInd))) ].
	
	"end part"
	(firstInd + replacementCollec size) to: (result size) do:
		[ :i | self assert: (result at: i) equals: (collection at: (secondInd + 1 - (firstInd + replacementCollec size) + i)) ]
]
