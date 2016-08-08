old-enumerating
with: aGrid collect: aBlock
	"aCollection must support #atRow:atColumn: and be at least as large as the receiver."

	^self withIndicesCollect: [:each :row :column |
		aBlock value: each value: (aGrid atRow: row atColumn: column)]
