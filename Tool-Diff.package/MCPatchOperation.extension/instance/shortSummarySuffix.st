*Tool-Diff
shortSummarySuffix
	"Answer a short summary suffix of the receiver."

	^self fromSource = self toSource
		ifTrue: [' (revision changed)']
		ifFalse: ['']