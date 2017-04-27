accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	| entry |
	(aReference isNull or: [ aReference globalName ~= self globalName])
		ifTrue: [ ^ absentBlockClosure value ].

	entry := entries
		at: aReference localName asInteger
		ifAbsent: [ ^ absentBlockClosure value ].
		
	^ presentBlockClosure cull: entry