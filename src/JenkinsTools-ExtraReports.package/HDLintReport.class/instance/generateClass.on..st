generating
generateClass: aClass on: aStream
	| sourceStream sourceName |
	sourceStream := WriteStream on: String new.
	sourceName := environment label , '-' , aClass name , '.st'.
	aStream tab; nextPutAll: '<file name="'; nextPutAll: (self encode: sourceName); nextPutAll: '">'; nextPut: Character lf.
	self generateClass: aClass source: sourceStream on: aStream.
	self generateClass: aClass class source: sourceStream on: aStream.
	aStream tab; nextPutAll: '</file>'; nextPut: Character lf.
	sourceName asFileReference ensureDelete writeStreamDo: [ :stream | 
		stream nextPutAll: sourceStream contents ]