stream protocol
peek
	cachedToken ifNil: [ cachedToken := self nextToken. ].
	
	^cachedToken	