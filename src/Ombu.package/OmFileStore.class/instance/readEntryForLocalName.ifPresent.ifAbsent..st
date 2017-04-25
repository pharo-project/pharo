accessing
readEntryForLocalName: aString ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	^ self entryPositionsByLocalName 
		at: aString
		ifPresent: [ :position |
			presentBlockClosure value: (self nextEntryFromPosition: position) ]
		ifAbsent: absentBlockClosure
