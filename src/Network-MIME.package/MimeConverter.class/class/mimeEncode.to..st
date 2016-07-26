convenience
mimeEncode: aCollectionOrStream to: outStream 
	self new
		dataStream: (aCollectionOrStream isStream 
				ifTrue: [ aCollectionOrStream ]
				ifFalse: [ aCollectionOrStream readStream ]);
		mimeStream: outStream;
		mimeEncode