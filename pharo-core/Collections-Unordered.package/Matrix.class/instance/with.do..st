enumerating
with: aCollection do: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	self withIndicesDo: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)].
