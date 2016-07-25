initialization
release
	"We're done with compression. Do some cleanup."
	literals := distances := literalFreq := distanceFreq := nil.
	self updateCrc.
	encoder flushBits.
	self writeFooter.