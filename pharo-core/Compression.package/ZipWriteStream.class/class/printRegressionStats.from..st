regression test
printRegressionStats: stats from: fd
	| raw compressed numFiles |
	raw := stats at: #rawSize ifAbsent:[0].
	raw = 0 ifTrue:[^self].
	compressed := stats at: #compressedSize ifAbsent:[0].
	numFiles := stats at: #numFiles ifAbsent:[0].
	Transcript cr; nextPutAll: fd fullName.
	Transcript cr; tab; nextPutAll:'Files compressed: ', numFiles asStringWithCommas.
	Transcript cr; tab; nextPutAll:'Bytes compressed: ', raw asStringWithCommas.
	Transcript cr; tab; nextPutAll:'Avg. compression ratio: ';
		print: ((compressed / raw asFloat * 100.0) truncateTo: 0.01).
	Transcript endEntry.