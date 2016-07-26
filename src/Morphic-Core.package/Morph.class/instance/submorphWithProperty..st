submorphs-accessing
submorphWithProperty: aSymbol
	^ submorphs detect: [:aMorph | aMorph hasProperty: aSymbol] ifNone: [nil]