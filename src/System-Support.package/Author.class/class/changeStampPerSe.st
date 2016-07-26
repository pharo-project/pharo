utilities
changeStampPerSe
	"Answer a string to be pasted into source code to mark who changed it and when."

	^ (Author fullNamePerSe ifNil: ['.']) , ' ' , Date today mmddyyyy, ' ',
		((String streamContents: [:s | Time now print24: true on: s]) 
			copyFrom: 1 to: 5)