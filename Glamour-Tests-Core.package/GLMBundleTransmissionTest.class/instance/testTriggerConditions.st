tests
testTriggerConditions 
	"Tests that the transmission is triggered only when the active origins change." 
	 
	| transmission port1 port2 | 
	transmission := GLMTransmission new. 
	port1 := GLMSimplePort new name: #port1. 
	port2 := GLMSimplePort new name: #port2. 
	transmission addActiveOrigin: port1. 
	transmission addPassiveOrigin: port2. 
	transmission destination: (GLMPort new name: #port3). 
	self assert: (transmission originatesAt: port1). 
	self assert: (transmission originatesAt: port2) not