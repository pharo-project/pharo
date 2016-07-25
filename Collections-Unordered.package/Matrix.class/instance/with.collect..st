enumerating
with: aCollection collect: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	^self withIndicesCollect: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)]
