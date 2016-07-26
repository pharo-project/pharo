scripting
sendToOutside: aPortName from: anOriginPortReference 
	"this is a shortcut method for exposing an inner port reference as an outer port name" 
	 
	self 
		sendTo: #outer -> aPortName 
		from: anOriginPortReference