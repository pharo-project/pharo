protected only for tests
withOrganizer: aNewOrganizer do: aBlock
	"Perform an action locally to aNewOrganizer. Does not impact any other organizers."
	
	| old |
	[ old := self organizer.
	old unregister.
	self organizer: aNewOrganizer.
	aNewOrganizer register.
	aBlock cull: aNewOrganizer] ensure: [ 
		self organizer: old. 
		old register.
		aNewOrganizer unregister] 