instruction decoding
popIntoLiteralVariable: value

	self pushLiteralVariable: value; doStore: statements