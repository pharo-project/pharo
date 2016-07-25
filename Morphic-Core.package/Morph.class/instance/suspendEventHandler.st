event handling
suspendEventHandler
	self eventHandler ifNotNil:
		[self setProperty: #suspendedEventHandler toValue: self eventHandler.
		self eventHandler: nil].
	submorphs do: [:m | m suspendEventHandler].  "All those rectangles"