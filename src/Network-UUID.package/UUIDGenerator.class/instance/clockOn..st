private
clockOn: stream
	"Write 8 bytes from the microsecond clock to stream"
	
	| microseconds |
	microseconds := Time microsecondClockValue.
	1 to: 8 do: [ :each |
		stream nextPut: (microseconds digitAt: each) ]