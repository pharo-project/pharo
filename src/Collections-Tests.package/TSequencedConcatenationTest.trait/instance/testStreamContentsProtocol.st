tests - streaming
testStreamContentsProtocol
	| result index |
	result:= self collectionClass << [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.