profiling
spyOn: aBlock
	"Profile system activity during execution of aBlock. 
	The argument is the desired samples per *milli* second.
	Mostly for polymorphism with MessageTally."
	self startProfiling.
	aBlock ensure: [ self stopProfiling ]
