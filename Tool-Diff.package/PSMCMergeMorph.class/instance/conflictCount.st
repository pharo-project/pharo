as yet unclassified
conflictCount
	"Answer the number of conflicts that are unresolved."

	^(self model ifNil: [^0]) conflicts count: [:c | c isResolved not]