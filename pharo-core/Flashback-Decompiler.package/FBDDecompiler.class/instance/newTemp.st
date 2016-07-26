private
newTemp	
	self incrTempCount.
	^ builder codeTemp: 'tmp' , tempCount printString