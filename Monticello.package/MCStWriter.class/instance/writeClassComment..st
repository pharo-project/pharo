writing
writeClassComment: definition

	self chunkContents: [ :str |
		stream bang.
		str nextPutAll: definition className;
			nextPutAll: ' commentStamp: ';
			store: definition commentStamp;
			nextPutAll: ' prior: 0' ].
	self basicChunkContents: [ :str | str nextPutAll: definition comment ].
	stream cr.