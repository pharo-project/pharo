private
handleLoadErrorsDuring: aBlock
	[aBlock
		on: InMidstOfFileinNotification do: [ :notification | notification resume: true ]]
		on: ClassBuilderWarning do: [ :error | error resume ]