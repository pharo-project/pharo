tests
testToPort
	browser transmit to: #one port: #port.
	self assert: browser transmissions first destination pane equals: browser panes first.
	self assert: browser transmissions first destination name equals: #port