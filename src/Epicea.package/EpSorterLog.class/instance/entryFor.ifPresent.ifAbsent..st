accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure
	"Answer an entry, evaluating either the first block closure if present or the second if absent."

	self entriesDo: [:each | 
		(self referenceTo: each) = aReference 
			ifTrue: [ ^ presentBlockClosure cull: each ] ].
	
	^ absentBlockClosure value