scripting
passivelyFrom: anOriginPortReference 
	"passivelyForm: is similar to from: only the origin will be a passive one. 
	in other words, a change in the origin port will not initiate a Transmission" 
	 
	self lastTransmission
		addPassiveOrigin: (self resolveOriginPort: anOriginPortReference)