tests
testAdd
	| composite |
	composite := GLMCompositePresentation new.
	composite add: GLMPresentation new.
	self assert: composite isEmpty not.
	self assert: composite presentations size equals: 1