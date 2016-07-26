*OpalCompiler-Core
isPushTemp: aPC
	^ (self method at: aPC) between: 16 and: 31