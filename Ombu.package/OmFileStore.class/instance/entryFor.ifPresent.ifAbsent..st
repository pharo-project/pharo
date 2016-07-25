accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	(aReference globalName = self globalName)
		ifFalse: [ ^ absentBlockClosure value ].

	^ entryPositionsByLocalName 
		at: aReference localName
		ifPresent: [ :position | presentBlockClosure value: (self nextEntryFromPosition: position) ]
		ifAbsent: [ absentBlockClosure value ]