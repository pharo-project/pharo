errors
nonInteractiveRequest: aStringOrText title: aTitle
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Interactive Request: '.
			aTitle ifNotNil: [ s nextPutAll: '[ '; nextPutAll: aTitle asString; nextPutAll: ' ] ' ].
			s nextPutAll: aStringOrText asString.
		])