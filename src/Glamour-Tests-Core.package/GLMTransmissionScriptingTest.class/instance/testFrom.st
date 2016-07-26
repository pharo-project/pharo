tests
testFrom
	browser transmit from: #one.
	self assert: browser transmissions first origins first pane equals: browser panes first.
	self assert: browser transmissions first origins first name equals: GLMPortIdentifier defaultOriginPortName