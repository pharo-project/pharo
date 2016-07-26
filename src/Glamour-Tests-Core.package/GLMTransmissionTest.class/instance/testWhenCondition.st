tests
testWhenCondition
	"Tests that the transmission is triggered only when the origin changes."

	| transmission port1 port2 |
	transmission := GLMTransmission new.
	port1 := (GLMSimplePort new)
		name: #port1;
		value: #value1.
	port2 := GLMSimplePort new name: #port2.
	transmission addActiveOrigin: port1.
	transmission destination: port2.
	transmission condition: [ :entity | entity = #value1 ].
	self assert: transmission meetsCondition.
	transmission transmit.
	self assert: port2 value equals: #value1