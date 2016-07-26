evaluating-private
writeArgumentNameAt: anIndex

	| argumentName |
	
	argumentName := self argumentNameAt: anIndex.
	stream
		nextPutAll: ' ';
		nextPutAll: argumentName;
		space