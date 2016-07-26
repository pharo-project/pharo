tests - copying with replacement
testCopyReplaceAllWithManyOccurence
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