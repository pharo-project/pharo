accessing
readEntryForLocalName: aString ifPresent: presentBlockClosure ifAbsent: absentBlockClosure

	^ (self blockForLocalNameAsInteger: aString asInteger)
			atLocalName: aString
			ifPresent: [ :position |
				presentBlockClosure value: (self nextEntryFromPosition: position) ]
			ifAbsent: absentBlockClosure
