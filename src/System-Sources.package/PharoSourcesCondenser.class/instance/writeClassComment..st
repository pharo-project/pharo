as yet unclassified
writeClassComment: aClass
	| organizer commentRemoteString stamp |
	
	organizer := aClass organization.
	commentRemoteString := organizer commentRemoteString.
	
	commentRemoteString isNil
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