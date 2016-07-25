initialization
initialize
	super initialize.
	self borderWidth: 1.
	self layoutInset: 0.
	self changeTableLayout.
	self hResizing: #shrinkWrap.
	self vResizing: #shrinkWrap.
	self color: self defaultBaseColor.
