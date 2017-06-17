printing
printDiff
	^ String new: 500 streamContents: [ :stream |
		self printDiffOn: stream ]