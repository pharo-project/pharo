tests
testCreation

	self deny: GLMNoBrowser new isNil.
	self assert: (GLMNoBrowser new) == GLMNoBrowser new