instruction decoding
pushClosureTemps: numTemps
	"push on stack nil numTemps times for the closure temps"
	self addBytecode: 'pushClosureTemps:' , numTemps printString