scripting
sendTo: aDesinationPortReference fromOutside: anOriginPortName 
	"this is a shortcut method for propagating inside an outer port name to an inner port reference" 
	 
	self 
		sendTo: aDesinationPortReference 
		from: #outer -> anOriginPortName