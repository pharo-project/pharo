printing
fullPrintOn: aStream threshold: perCent
	| threshold |  
	threshold := (perCent asFloat / 100 * tally) rounded.
	aStream nextPutAll: '**Tree**'; cr.
	self
		rootPrintOn: aStream
		total: tally
		totalTime: time
		threshold: threshold.
	aStream nextPut: Character newPage; cr.
	aStream nextPutAll: '**Leaves**'; cr.
	self
		leavesPrintOn: aStream
		threshold: threshold