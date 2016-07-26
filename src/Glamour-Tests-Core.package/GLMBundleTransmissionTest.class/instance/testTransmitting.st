tests
testTransmitting
	"Tests that the origins are set up properly and that their order is retained."

	| transmission |
	transmission := GLMTransmission new.
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port1;
				value: 1).
	transmission destination: (GLMSimplePort new name: #port5).
	self assert: transmission value equals: 1.
	self assert: transmission destination value isNil.
	transmission transmit.
	self assert: transmission destination value equals: 1.
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port2;
				value: 2).
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port3;
				value: 3).
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port4;
				value: 4).
	self assert: transmission value equals: (GLMMultiValue withAll: #(1 2 3 4)).
	transmission transmit.
	self assert: transmission destination value equals: (GLMMultiValue withAll: #(1 2 3 4))