tests
testExplicitTransmission
	| finder |
	finder := GLMFinder new.
	finder transmit fromOutsidePort: #entity; toOutsidePort: #selection.
	finder startOn: 42.
	self assert: (finder pane port: #selection) value equals: 42