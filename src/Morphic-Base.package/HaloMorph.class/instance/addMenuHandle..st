handles
addMenuHandle: haloSpec
	self addHandle: haloSpec on: #mouseDown send: #doMenu:with: to: self