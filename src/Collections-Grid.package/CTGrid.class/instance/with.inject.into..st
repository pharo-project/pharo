old-enumerating
with: aCollection inject: startingValue into: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	^ self withIndicesInject: startingValue into: [:value :each :row :column |
			aBlock value: value value: each value: (aCollection atRow: row atColumn: column)]