method
browseMessagesFrom: aSelector
	" badly named, it browses implementors "

	self getSelectorAndSendQuery: #browseAllImplementorsOf: to: self systemNavigation with: {} selector: aSelector