tests
testPorts
	"Tests that the origins are set up properly and that their order is retained."

	| transmission |
	transmission := GLMTransmission new.
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port1;
				value: 1).
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
	transmission destination: (GLMSimplePort new name: #port5).
	self assert: transmission originReferences size equals: 4.
	transmission originReferences do: [ :each | self assert: (each isKindOf: GLMOriginPortReference) ].
	self assert: (transmission originReferences at: 1) port name equals: #port1.
	self assert: (transmission originReferences at: 2) port name equals: #port2.
	self assert: (transmission originReferences at: 3) port name equals: #port3.
	self assert: (transmission originReferences at: 4) port name equals: #port4.
	self assert: transmission origins size equals: 4.
	transmission origins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission origins at: 1) name equals: #port1.
	self assert: (transmission origins at: 2) name equals: #port2.
	self assert: (transmission origins at: 3) name equals: #port3.
	self assert: (transmission origins at: 4) name equals: #port4.
	self assert: transmission activeOrigins size equals: 2.
	transmission activeOrigins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission activeOrigins at: 1) name equals: #port1.
	self assert: (transmission activeOrigins at: 2) name equals: #port3.
	self assert: transmission passiveOrigins size equals: 2.
	transmission passiveOrigins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission passiveOrigins at: 1) name equals: #port2.
	self assert: (transmission passiveOrigins at: 2) name equals: #port4.
	self assert: (transmission destination isKindOf: GLMSimplePort).
	self assert: transmission destination name equals: #port5