private
updateInsetAndSpacing

	self triggerBrick margin: {
		self inset top.
		self inset right.
		0.
		self inset left }.

	self contentBrick margin: {
		self spacing.
		self inset right.
		self inset bottom.
		self inset left }