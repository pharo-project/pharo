scheduling
flush

	process ifNotNil: [
		"Execute deferred action and clean up"
		[ self sendMessage ] ensure: [ process := nil ] 
	].