final steps
reportSizes
	| oldAvgBytes "newAvgBytes" newHeapSize newHeap |
	newHeapSize := 20 * 1024 * 1024.
	newHeap := espellBackend objectMemory.
	Transcript
		nextPutAll: 'done.'; cr;
		nextPutAll: 'initial heap size: '; nextPutAll: newHeapSize asStringWithCommas; cr;
"		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush.
	newHeapSize := newHeap endOfMemory
					- newHeap scavenger eden limit
					- newHeap totalFreeListBytes.
"	newAvgBytes := newHeapSize asFloat / newHeapNumObjs."
	Transcript
		nextPutAll: 'final new heap size: '; nextPutAll: newHeapSize asStringWithCommas; tab;
	"	nextPutAll: ' (avg obj bytes '; print: newAvgBytes maxDecimalPlaces: 2; nextPutAll: ' words '; print: newAvgBytes / self wordSize maxDecimalPlaces: 2; nextPut: $); cr;
""		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush