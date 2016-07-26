instance creation
acceptFrom: aSocket
	^[ super new acceptFrom: aSocket ]
		repeatWithGCIf: [ :sock | sock isValid not ]