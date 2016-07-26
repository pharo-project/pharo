tests
testClose
	| finder |
	finder := GLMFinder new.
	window := finder openOn: 123.
	self assert: window owner notNil.
	finder close.
	self assert: window owner isNil