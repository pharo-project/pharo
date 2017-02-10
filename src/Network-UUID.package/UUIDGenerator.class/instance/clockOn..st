private
clockOn: stream
	| microseconds |
	microseconds := Time microsecondClockValue.
	1 to: 8 do: [ :each |
		stream nextPut: (microseconds digitAt: each) ]