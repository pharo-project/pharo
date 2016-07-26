layout
layoutWidgets
	| inner outer |
	outer := super innerBounds.
	inner := self innerBounds.
	transform bounds: inner.
	scrollBar bounds: ((inner left @ inner bottom)
						corner: outer bottomRight)