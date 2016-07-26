type of methods
fromActiveToHistorical
	"If the receiver was generated as an active method, it can be converted to a historical one by reading the data of the compiled method (if exists)"
	
	self isActive ifTrue: [
		self asHistorical ]