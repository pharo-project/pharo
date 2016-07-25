handles
addHelpHandle: haloSpec
	target balloonText ifNotNil:
		[(self addHandle: haloSpec on: #mouseDown send: #mouseDownOnHelpHandle: to: innerTarget)
			on: #mouseUp send: #deleteBalloon to: innerTarget]
