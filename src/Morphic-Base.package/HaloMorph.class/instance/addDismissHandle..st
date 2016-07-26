handles
addDismissHandle: handleSpec 
	"Add the dismiss handle according to the spec, unless my target resists
	dismissal "
	| dismissHandle |
	target okayToAddDismissHandle
		ifTrue: [dismissHandle := self
						addHandle: handleSpec
						on: #mouseDown
						send: #mouseDownInDimissHandle:with:
						to: self.
			dismissHandle 
				on: #mouseUp
				send: #maybeDismiss:with:
				to: self.
			dismissHandle
				on: #mouseDown
				send: #setDismissColor:with:
				to: self.
			dismissHandle
				on: #mouseMove
				send: #setDismissColor:with:
				to: self]