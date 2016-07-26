actions
refresh
	[ UIManager default 
		informUser: 'Refreshing catalog (invalidating cache)...'
		during: [ self provider reset ] ]
	on: Error do: [ :e |
		e crLog.
		self inform: 'I cannot refresh catalog list. Please review your internet connection and try again!' ]