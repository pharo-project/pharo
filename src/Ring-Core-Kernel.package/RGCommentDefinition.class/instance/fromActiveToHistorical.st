type of comments
fromActiveToHistorical
	"If the receiver was generated as an active comment, it can be converted to a historical one by reading the data of the real class (if exists)"
	
	self isActive ifTrue: [
		self asHistorical ]