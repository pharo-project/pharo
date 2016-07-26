tests
testFromPort
	browser transmit from: #one port: #port.
	self assert: browser transmissions first origins first pane equals: browser panes first.
	self assert: browser transmissions first origins first name equals: #port