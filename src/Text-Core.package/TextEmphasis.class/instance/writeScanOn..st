styling
writeScanOn: strm

	emphasisCode = 1 ifTrue: [strm nextPut: $b].
	emphasisCode = 2 ifTrue: [strm nextPut: $i].
	emphasisCode = 0 ifTrue: [strm nextPut: $n].
	emphasisCode = 16 ifTrue: [strm nextPut: $=].
	emphasisCode = 4 ifTrue: [strm nextPut: $u].