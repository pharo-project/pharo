scripting
from: anOriginPortReference 
	"send this message after showOn: to specify an origin for the Transmission. 
	you can cascade several from: to specify additional origins" 
	 
	self lastTransmission 
		addActiveOrigin: (self resolveOriginPort: anOriginPortReference)