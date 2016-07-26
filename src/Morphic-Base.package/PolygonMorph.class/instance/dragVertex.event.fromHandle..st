editing
dragVertex: ix event: evt fromHandle: handle
	| p |
	p := evt cursorPoint.
	handle position: p - (handle extent // 2).
	self verticesAt: ix put: p