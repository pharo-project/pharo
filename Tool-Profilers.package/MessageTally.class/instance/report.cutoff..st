reporting
report: strm cutoff: threshold 
	tally = 0
		ifTrue: [strm nextPutAll: ' - no tallies obtained']
		ifFalse: 
			[strm nextPutAll: ' - '; print: tally; nextPutAll: ' tallies, ', time printString, ' msec.'; cr; cr.
			self fullPrintOn: strm threshold: threshold].
		
	time isZero ifFalse:	
		[self reportGCStatsOn: strm].