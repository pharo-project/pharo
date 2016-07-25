private
mouseDownInDimissHandle: evt with: dismissHandle
	evt hand obtainHalo: self.
	self removeAllHandlesBut: dismissHandle.
	self setColor: Color darkGray toHandle: dismissHandle.
