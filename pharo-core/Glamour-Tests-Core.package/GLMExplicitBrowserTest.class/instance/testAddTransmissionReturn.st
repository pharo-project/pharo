tests
testAddTransmissionReturn
	| transmission |
	transmission := GLMTransmission new.
	self assert: (GLMExplicitBrowser new addTransmission: transmission) == transmission