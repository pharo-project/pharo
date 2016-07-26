private
logTitle: aString andDescription: aStringOrText to: aStream
	
	aStream
		nextPutAll: aString;
		nextPutAll: ': ';
		nextPutAll: aStringOrText asString;
		cr.