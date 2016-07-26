converting
surroundedBy: aString
	^ self class streamContents: [ :s|
		s nextPutAll: aString.
		s nextPutAll: self.
		s nextPutAll: aString ].