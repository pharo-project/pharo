accessing
messageText
	^String streamContents: [ :strm |
		messageText ifNotNil: [ strm nextPutAll: messageText; space ].
		self errorCode isZero ifFalse: [
			strm nextPutAll: '[error '; print: self errorCode; nextPutAll: '][';
				nextPutAll: self errorString;
				nextPut: $] ]]