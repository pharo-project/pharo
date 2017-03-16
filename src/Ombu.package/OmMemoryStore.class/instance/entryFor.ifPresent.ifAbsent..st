accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	| entry |
	(aReference globalName = self globalName)
		ifFalse: [ ^ absentBlockClosure value ].

	entry := entries
		at: aReference localName asInteger
		ifAbsent: [ ^ absentBlockClosure value ].
		
	^ presentBlockClosure cull: entry