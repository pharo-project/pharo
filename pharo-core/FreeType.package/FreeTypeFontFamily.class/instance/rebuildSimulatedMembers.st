simulated members
rebuildSimulatedMembers
	"FOR TESTING ONLY"
	
	members := members reject:[:each| each simulated].
	self addSimulatedMembers.