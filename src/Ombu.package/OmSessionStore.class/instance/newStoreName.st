private
newStoreName

	^ String streamContents: [ :stream |
		stream 
			nextPutAll: Smalltalk imageFile basenameWithoutExtension;
			nextPut: $_;
			nextPutAll: (UUID new asString36 first: 7) ]