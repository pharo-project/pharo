"
A trait for test purposes
"
Trait {
	#name : #TCopySequenceableWithReplacement,
	#category : 'Collections-Tests-Abstract'
}

{ #category : #requirements }
TCopySequenceableWithReplacement >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	self explicitRequirement
]

{ #category : #requirements }
TCopySequenceableWithReplacement >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	self explicitRequirement
]

{ #category : #'tests - copying with replacement' }
TCopySequenceableWithReplacement >> firstIndexesOf: aSubCollection in: collection [
" return an OrderedCollection with the first indexes of the occurrences of subCollection in  collection "
	| tmp result currentIndex |
	tmp:= collection.
	result:= OrderedCollection new.
	currentIndex := 1.
	
	[tmp isEmpty ]whileFalse:
		[
		(tmp beginsWith: aSubCollection)
			ifTrue: [ 	
				result add: currentIndex.
				1 to: aSubCollection size do: 
					[:i | 
					tmp := tmp copyWithoutFirst.
					currentIndex := currentIndex + 1]
				]
			ifFalse: [
				tmp := tmp copyWithoutFirst.
				currentIndex := currentIndex +1.
				]
		 ].
	
	^ result.
	
]

{ #category : #'tests - fixture' }
TCopySequenceableWithReplacement >> howMany: aSubCollection in: collection [
" return an integer representing how many time 'subCollection'  appears in 'collection'  "
	| tmp nTime |
	tmp := collection.
	nTime:= 0.
	
	[tmp isEmpty ]whileFalse:
		[
		(tmp beginsWith: aSubCollection)
			ifTrue: [ 	
				nTime := nTime + 1.
				1 to: aSubCollection size do: [:i | tmp := tmp copyWithoutFirst.]
				]
			ifFalse: [tmp := tmp copyWithoutFirst.]
		 ].
	
	^ nTime.
	
]

{ #category : #requirements }
TCopySequenceableWithReplacement >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	self explicitRequirement
]

{ #category : #requirements }
TCopySequenceableWithReplacement >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TCopySequenceableWithReplacement >> test0FixtureCopyWithReplacementTest [
	self replacementCollection.
	self oldSubCollection.
	self collectionWith1TimeSubcollection.
	self assert: (self howMany: self oldSubCollection in: self collectionWith1TimeSubcollection) = 1
]

{ #category : #'tests - copying with replacement' }
TCopySequenceableWithReplacement >> testCopyReplaceAllWith1Occurence [
	| result  firstIndexesOfOccurrence index endPartIndexResult endPartIndexCollection |
	
	result := self collectionWith1TimeSubcollection  copyReplaceAll: self oldSubCollection with: self replacementCollection .
	
	"detecting indexes of olSubCollection"
	firstIndexesOfOccurrence  := self firstIndexesOf: self oldSubCollection in: self collectionWith1TimeSubcollection .
	index:= firstIndexesOfOccurrence at: 1.
	
	"verify content of 'result' : "
	"first part of 'result'' : '"

	1 to: (index -1) do: 
		[
		:i |  
		self assert: (self collectionWith1TimeSubcollection  at:i)=(result at: i)
		].

	" middle part containing replacementCollection : "
	
	index to: (index + self replacementCollection size-1) do: 
		[
		:i |
		self assert: ( result at: i )=(self replacementCollection at: ( i - index + 1 ))
		].
	
	" end part :"
	
	endPartIndexResult :=  index + self replacementCollection  size .
	endPartIndexCollection :=   index + self oldSubCollection size  .
	
	1 to: (result size - endPartIndexResult - 1 ) do:
		[ 
		:i |
		self assert: (result at: ( endPartIndexResult + i - 1 ) ) = (self collectionWith1TimeSubcollection  at: ( endPartIndexCollection + i - 1 ) ).
		].
	
	
	
]

{ #category : #'tests - copying with replacement' }
TCopySequenceableWithReplacement >> testCopyReplaceAllWithManyOccurence [
	| result firstIndexesOfOccurrence resultBetweenPartIndex collectionBetweenPartIndex diff |
	" testing fixture here as this method may be not used for collection that can't contain equals element :"
	self collectionWith2TimeSubcollection.
	self assert: (self howMany: self oldSubCollection in: self collectionWith2TimeSubcollection) = 2.	" test :"
	diff := self replacementCollection size - self oldSubCollection size.
	result := self collectionWith2TimeSubcollection copyReplaceAll: self oldSubCollection with: self replacementCollection.	"detecting indexes of olSubCollection"
	firstIndexesOfOccurrence := self firstIndexesOf: self oldSubCollection in: self collectionWith2TimeSubcollection.	" verifying that replacementCollection has been put in places of oldSubCollections "
	firstIndexesOfOccurrence
		do: [ :each | 
			(firstIndexesOfOccurrence indexOf: each) = 1
				ifTrue: [ each to: self replacementCollection size do: [ :i | self assert: (result at: i) = (self replacementCollection at: i - each + 1) ] ]
				ifFalse: [ each + diff to: self replacementCollection size do: [ :i | self assert: (result at: i) = (self replacementCollection at: i - each + 1) ] ] ].	" verifying that the 'between' parts correspond to the initial collection : "
	1 to: firstIndexesOfOccurrence size do: [ :i | 
		i = 1
			ifTrue: [ 1 to: (firstIndexesOfOccurrence at: i) - 1 do: [ :j | self assert: (result at: i) = (self collectionWith2TimeSubcollection at: i) ] ]
			ifFalse: [ 
				resultBetweenPartIndex := (firstIndexesOfOccurrence at: i - 1) + self replacementCollection size.
				collectionBetweenPartIndex := (firstIndexesOfOccurrence at: i - 1) + self oldSubCollection size.
				1 to: (firstIndexesOfOccurrence at: i) - collectionBetweenPartIndex - 1 do: [ :j | 
					self
						assert:
							(result at: resultBetweenPartIndex + i - 1)
								= (self collectionWith2TimeSubcollection at: collectionBetweenPartIndex + i - 1) ] ]	" specific comportement for the begining of the collection :"	" between parts till the end : " ].	"final part :"
	1 to: self collectionWith2TimeSubcollection size - (firstIndexesOfOccurrence last + self oldSubCollection size) do: [ :i | 
		self
			assert:
				(result at: firstIndexesOfOccurrence last + self replacementCollection size - 1 + i)
					= (self collectionWith2TimeSubcollection at: firstIndexesOfOccurrence last + self oldSubCollection size - 1 + i) ]
]

{ #category : #'tests - copying with replacement' }
TCopySequenceableWithReplacement >> testCopyReplaceFromToWith [
	| result  indexOfSubcollection lastIndexOfOldSubcollection lastIndexOfReplacementCollection |
	
	indexOfSubcollection := (self firstIndexesOf: self oldSubCollection  in:  self collectionWith1TimeSubcollection) at: 1. 
	lastIndexOfOldSubcollection := indexOfSubcollection + self oldSubCollection size -1.
	lastIndexOfReplacementCollection := indexOfSubcollection + self replacementCollection  size -1.
	
	result := self collectionWith1TimeSubcollection  copyReplaceFrom: indexOfSubcollection  to: lastIndexOfOldSubcollection   with: self replacementCollection .
	
	"verify content of 'result' : "
	"first part of 'result'  "
	
	1 to: (indexOfSubcollection  - 1) do: 
		[ 
		:i | 
		self assert: (self collectionWith1TimeSubcollection  at:i) = (result at: i)
		].
	
	" middle part containing replacementCollection : "
	
	(indexOfSubcollection ) to: ( lastIndexOfReplacementCollection  ) do: 
		[
		:i |
		self assert: (result at: i)=(self replacementCollection at: (i - indexOfSubcollection +1))
		].
	
	" end part :"
	1 to: (result size - lastIndexOfReplacementCollection   ) do:
		[ 
		:i |
		self assert: (result at: ( lastIndexOfReplacementCollection  + i  ) ) = (self collectionWith1TimeSubcollection  at: ( lastIndexOfOldSubcollection  + i  ) ).
		].
	
	
	

	
	
]

{ #category : #'tests - copying with replacement' }
TCopySequenceableWithReplacement >> testCopyReplaceFromToWithInsertion [
	| result  indexOfSubcollection |
	
	indexOfSubcollection := (self firstIndexesOf: self oldSubCollection  in:  self collectionWith1TimeSubcollection) at: 1. 
	
	result := self collectionWith1TimeSubcollection  copyReplaceFrom: indexOfSubcollection  to: ( indexOfSubcollection - 1 ) with: self replacementCollection .
	
	"verify content of 'result' : "
	"first part of 'result'' : '"
	
	1 to: (indexOfSubcollection -1) do: 
		[
		:i | 
		self assert: (self collectionWith1TimeSubcollection  at:i)=(result at: i)
		].
	
	" middle part containing replacementCollection : "
	indexOfSubcollection  to: (indexOfSubcollection  + self replacementCollection size-1) do: 
		[
		:i |
		self assert: ( result at: i )=(self replacementCollection at: ( i - indexOfSubcollection +1 ))
		].
	
	" end part :"
	(indexOfSubcollection  + self replacementCollection size) to: (result size) do:
		[:i|
		self assert: (result at: i)=(self collectionWith1TimeSubcollection  at: (i-self replacementCollection size))].
	
	" verify size: "	
	self assert: result size=(self collectionWith1TimeSubcollection  size + self replacementCollection size).
	

	

	
	
]
