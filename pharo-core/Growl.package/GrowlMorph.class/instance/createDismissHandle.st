initialization
createDismissHandle
	| handle |
	handle := self theme growlDismissHandleFor: self.
	handle on: #mouseUp send: #delete to: self.
	^ handle