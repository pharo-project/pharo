event handling
restoreSuspendedEventHandler
	| savedHandler |
	(savedHandler := self valueOfProperty: #suspendedEventHandler) ifNotNil:
		[self eventHandler: savedHandler].
	submorphs do: [:m | m restoreSuspendedEventHandler]
