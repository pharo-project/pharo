communicating
sliceNotification
	^ String streamContents: [ :str |
		str
			nextPutAll: 'Fix in inbox: ';
			nextPutAll: self slice workingCopy versionInfo name;
		cr;
		cr;
		nextPutAll: self changesFromSliceCommitMessage ]