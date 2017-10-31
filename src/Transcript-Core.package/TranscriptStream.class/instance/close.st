streaming
close

	self 
	flush;
	critical: [ 
		outputs do: [ :sub | sub transcriptClosed: self ].
		stream close 
	]
	