initialization
stationarySetup

	self actWhen: #startDrag.
	self cornerStyle: #rounded.
	self borderNormal.
	self on: #mouseEnter send: #borderThick to: self.
	self on: #mouseDown send: nil to: nil.
	self on: #mouseLeave send: #borderNormal to: self.
	self on: #mouseLeaveDragging send: #borderNormal to: self.
	self on: #mouseUp send: #borderThick to: self.