private - 1 writing
writeClassComment: aClass
	| organizer commentRemoteString stamp |
	
	organizer := aClass organization.
	commentRemoteString := organizer commentRemoteString.
	
	(commentRemoteString isNil or: [ commentRemoteString sourceFileNumber = 1 ])
		ifTrue: [ ^ self ].
		
	self nextCommentChunkDo: [ :strm |
		strm 
			nextPutAll: aClass name;
			nextPutAll: ' commentStamp: '.
		stamp := organizer commentStamp 
			ifNil: ['<historical>'].
		stamp storeOn: strm ].
	
	self 
		writeRemoteString: organizer classComment
		for: aClass