private
prepareFilterInContext: aSpotterContext
	super prepareFilterInContext: aSpotterContext.
	
	lowerThreshold ifNil: [ lowerThreshold := self defaultLowerThreshold ]. " performance optimization "
	upperThreshold ifNil: [ upperThreshold := self defaultUpperThreshold ]. " performance optimization "