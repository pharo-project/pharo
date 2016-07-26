tests
testAddTransmissions
	| browser transmission |
	browser := GLMBrowser new.
	self assert: browser transmissions isEmpty.
	transmission := GLMTransmission new.
	browser addTransmission: transmission.
	self assert: transmission browser equals: browser