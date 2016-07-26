registration
unregisterHandler: anHandledId 
	categories do: [ :each | each unregisterHandler: anHandledId ]