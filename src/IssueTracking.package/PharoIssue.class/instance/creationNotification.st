communicating
creationNotification
	^ String streamContents: [ :str |
		str nextPutAll: self headerString;
		cr;
		print: self trackerUrl ]