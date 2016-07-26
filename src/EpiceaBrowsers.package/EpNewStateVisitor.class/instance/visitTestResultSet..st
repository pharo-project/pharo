visitor
visitTestResultSet: anEvent

	"FIX"
	^ String streamContents: [ :stream |
		anEvent classNamesAndSelectorsAndKinds do: [ :each |
				stream
					nextPutAll: each first;
					nextPutAll: '»';
					nextPutAll: each second;
					nextPutAll: ' [';
					nextPutAll: each third;
					nextPut: $] ]
			separatedBy: [ 
				stream cr ] ]