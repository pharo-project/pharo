accessing
MinCycleLapse: milliseconds
	"set the minimum amount of time that may transpire between two calls to doOneCycle"
	MinCycleLapse := milliseconds ifNotNil: [ milliseconds rounded ].