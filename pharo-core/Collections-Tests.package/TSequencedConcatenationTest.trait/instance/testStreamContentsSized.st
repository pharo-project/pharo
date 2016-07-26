tests - streaming
testStreamContentsSized
	| result |
	result:= self collectionClass new: 1 streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.
	
	result:= self collectionClass new: 1000 streamContents: [ :s|
		s 
			nextPutAll: self firstCollection;
			nextPutAll: self secondCollection ].
	self assert: result equals: self firstCollection, self secondCollection.