tests
testTextPortIsPopulatedByDefault
	| composite |
	composite := (GLMCompositePresentation new with: [:a | a text ]) startOn: '42'.
	self assert: composite presentations first text equals: '42'