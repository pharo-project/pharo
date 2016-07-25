accessing
setAmountVisible: visible all: all

	self amountBrick text: (visible asString, '/', all asString).
	self amountBrick wrappedBounds markWidthDirty.
	self amountBrick doLayout.
	self doLayoutForce