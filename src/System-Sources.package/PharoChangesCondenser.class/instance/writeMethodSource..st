private - 1 writing
writeMethodSource: aMethod
	
	self nextCommentChunkDo: [ :strm |
		strm 
			nextPutAll: aMethod methodClass name;
			nextPutAll: ' methodsFor: ';
			store: aMethod category asString;
			nextPutAll: ' stamp: ';
			store: aMethod timeStamp ].
	
	self
		writeRemoteString: aMethod sourceCode
		for: aMethod.
		
	stream nextPutAll: ' !'; cr.