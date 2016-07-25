tests
testSimple
	GLMPresentation new announce: GLMContextChanged new.
	self assert: GLMLogger instance announcements size equals: 1