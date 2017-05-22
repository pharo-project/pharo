printing
printOn: aStream
	aStream print: self class;
		nextPut: $(;
		nextPutAll: self package name.

	commit ifNotNil: [ 
		aStream 
		nextPutAll: ', loaded from: ';
		nextPutAll: commit shortId ].
		
	originatingCommit ifNotNil: [ 
		aStream 
			nextPutAll: ', committed in: ';
			nextPutAll: originatingCommit shortId ].
		
	aStream nextPut: $).
