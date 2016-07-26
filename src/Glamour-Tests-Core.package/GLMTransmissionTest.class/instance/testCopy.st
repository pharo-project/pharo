tests
testCopy
	| transmission port1 port2 newTransmission |
	transmission := GLMTransmission new.
	port1 := GLMSimplePort new.
	port2 := GLMSimplePort new.
	transmission destination: port2.
	transmission addActiveOrigin: port1.
	newTransmission := transmission copy.
	self assert: newTransmission destination ~~ transmission destination.
	self assert: newTransmission origins first ~~ transmission origins first.