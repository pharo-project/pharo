initialization
initialize
	"Some magic numbers to get the process started"
	state := OrderedCollection newFrom: { 
			(ThirtyTwoBitRegister new load: 1732584193).
			(ThirtyTwoBitRegister new load: 4023233417).
			(ThirtyTwoBitRegister new load: 2562383102).
			(ThirtyTwoBitRegister new load: 271733878)
		 }