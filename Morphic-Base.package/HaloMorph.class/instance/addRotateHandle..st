handles
addRotateHandle: haloSpec
	(self addHandle: haloSpec on: #mouseDown send: #startRot:with: to: self)
		on: #mouseMove send: #doRot:with: to: self

