accessing
acceptContents: aString

	^ (self entity acceptContents: aString)
		ifTrue: [  
			self update.
			true]