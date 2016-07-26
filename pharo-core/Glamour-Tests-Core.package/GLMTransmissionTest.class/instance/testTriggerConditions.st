tests
testTriggerConditions 
	"Tests that the transmission is triggered only when the origin changes." 
	
	| transmission port1 port2 | 
	transmission := GLMTransmission new. 
	port1 := GLMSimplePort new name: #port1. 
	port2 := GLMSimplePort new name: #port2. 
	transmission addActiveOrigin: port1. 
	transmission destination: port2. 
	self assert: (transmission originatesAt: port1). 
	self assert: (transmission originatesAt: port2) not. 
	self assert: (transmission originatesAt: GLMPort new) not