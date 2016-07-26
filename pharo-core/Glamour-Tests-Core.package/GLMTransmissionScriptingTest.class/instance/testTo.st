tests
testTo
	browser transmit to: #one.
	self assert: browser transmissions first destination pane equals: browser panes first.
	self assert: browser transmissions first destination name equals: GLMPortIdentifier defaultTargetPortName