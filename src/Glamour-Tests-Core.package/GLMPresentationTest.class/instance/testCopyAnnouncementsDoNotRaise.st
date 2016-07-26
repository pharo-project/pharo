tests
testCopyAnnouncementsDoNotRaise
	| presentation newPresentation |
	presentation := GLMPresentation new.
	presentation when: GLMContextChanged do: [ :ann | self fail ].
	self assert: presentation numberOfSubscriptions equals: 1.
	newPresentation := presentation copy.
	newPresentation announce: GLMContextChanged.
	self assert: newPresentation numberOfSubscriptions equals: 0