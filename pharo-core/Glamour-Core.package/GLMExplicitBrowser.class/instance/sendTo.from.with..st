scripting
sendTo: aDesinationPortReference from: anOriginPortReference with: aBlock 
	"this message links two port references with a transmission that holds no presentation" 
	 
	self transmissions 
		addLast: 
			((GLMTransmission new) 
				addActiveOrigin: (self resolveOriginPort: anOriginPortReference); 
				destination: (self resolveDestinationPort: aDesinationPortReference); 
				transformation: aBlock; 
				yourself)