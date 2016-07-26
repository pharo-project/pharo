tests
testEquality
	| presentation |
	presentation := GLMPresentation new.
	self assert: (presentation = presentation copy) not