refreshing
statusString

	^ String streamContents: [:stream |
		| filtered total |
		filtered := logBrowserModel filteredEntriesCount.
		total := logBrowserModel totalEntriesCount.
		
		stream
			print: filtered;
			nextPutAll: ' events'.
	
		filtered = total ifFalse: [
			stream
				nextPutAll: ' (';
				print: total;
				nextPutAll: ' total)' ].
		]