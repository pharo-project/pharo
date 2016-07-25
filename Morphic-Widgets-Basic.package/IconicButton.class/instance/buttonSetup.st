initialization
buttonSetup
	self actWhen: #buttonUp.
	self cornerStyle: #rounded.
	self borderNormal.
	self on: #mouseEnter send: #borderRaised to: self.
	self on: #mouseLeave send: #borderNormal to: self.
	self on: #mouseLeaveDragging send: #borderNormal to: self.
	self on: #mouseDown send: #borderInset to: self.
	self on: #mouseUp send: #borderRaised to: self.