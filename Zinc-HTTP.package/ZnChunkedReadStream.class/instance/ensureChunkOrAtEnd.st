private
ensureChunkOrAtEnd
	atEnd 
		ifTrue: [ ^ self ].
	(chunk isNil or: [ position >= limit ]) 
		ifTrue: [ self getNextChunk ]
