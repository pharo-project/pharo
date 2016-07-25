tests - streaming
testStreamContents
	| result index |
	result:= self collectionClass streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.