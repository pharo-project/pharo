accessing
description
	^ String streamContents: [ :s |
		s nextPutAll: 'An attempt to use interactive tools detected, while in non-interactive mode'. 
		self messageText  ifNotEmpty: [ :message| 
			s cr; tab; nextPutAll: message ].
		self exception ifNotNil: [ :ex |
			s cr; tab; print: ex ]].