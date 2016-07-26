printing
printOn: aStream indent: level

	aStream nextPutAll: '^ '. 
	expr printOn: aStream indent: level.
	expr printCommentOn: aStream indent: level