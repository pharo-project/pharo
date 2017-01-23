accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure
	"Look for an entry, evaluating either the first block closure if present or the second if absent."

	^ absentBlockClosure value