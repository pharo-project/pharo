initialization
finish
	"Finish pending operation. Do not close output stream."
	self deflateBlock.
	self flushBlock: true.
	encoder flush.