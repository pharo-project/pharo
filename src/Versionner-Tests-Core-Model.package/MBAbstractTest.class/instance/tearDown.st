running
tearDown
	super tearDown.
	configuration := nil.
	self removeClassIfExist: self configurationName asSymbol
