as yet unclassified
writeScanOn: strm

	kern > 0 ifTrue: [
		1 to: kern do: [:kk | strm nextPut: $+]].
	kern < 0 ifTrue: [
		1 to: 0-kern do: [:kk | strm nextPut: $-]].