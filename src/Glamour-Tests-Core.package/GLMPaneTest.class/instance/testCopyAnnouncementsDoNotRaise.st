tests
testCopyAnnouncementsDoNotRaise
	| pane newPane |
	pane := GLMPane new.
	pane when: GLMPresentationsChanged do: [ :ann | self fail ].
	self assert: pane numberOfSubscriptions equals: 1.
	newPane := pane copy.
	newPane announce: GLMPresentationsChanged.
	self assert: newPane numberOfSubscriptions equals: 0