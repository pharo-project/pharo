private
prepareProcessorInContext: aContext
	super prepareProcessorInContext: aContext.
	
	self hasDynamicItems ifTrue: [ allCandidates := nil ]