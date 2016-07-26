tests
testCopy
	| transmission newTransmission | 
	transmission := GLMTransmission new. 
	transmission addActiveOrigin: ((GLMSimplePort new) name: #port1; value: 1). 
	transmission addPassiveOrigin: ((GLMSimplePort new) name: #port2; value: 2). 
	transmission addActiveOrigin: ((GLMSimplePort new) name: #port3; value: 3). 
	transmission addPassiveOrigin: ((GLMSimplePort new) name: #port4; value: 4). 
	transmission destination: (GLMSimplePort new name: #port5). 

	newTransmission := transmission copy.
	
	self assert: transmission originReferences first ~= newTransmission originReferences first.