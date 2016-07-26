tests - enumerating
testFlatCollect

	self assert: (self simpleCollection flatCollect: [ :x | { x } ]) equals: self simpleCollection.
	self assert: (self simpleCollection flatCollect: [ :x | { x } ]) species = self simpleCollection species.
	
	self 
		assert: (self collectionOfCollectionsOfInts flatCollect: [ :x | { x } ]) 
		equals: self collectionOfCollectionsOfInts.
	self 
		assert: (self collectionWithCharacters flatCollect: [ :x | { x } ]) 
		equals: self collectionWithCharacters.
	self 
		assert: (self collectionOfCollectionsOfStrings flatCollect: [ :x | { x } ]) 
		equals: self collectionOfCollectionsOfStrings