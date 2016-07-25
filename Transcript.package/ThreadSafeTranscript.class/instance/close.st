streaming
close
	self flush.
	self critical: [ stream close ]
	