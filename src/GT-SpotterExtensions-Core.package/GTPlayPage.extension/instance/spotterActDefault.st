*GT-SpotterExtensions-Core
spotterActDefault
	| result |
	result := GTPlayground new openOn: self.
	"Spotter only sets the title without installing the stash persistency.
	This is because when openinng the Playground the changing of the text triggers a save and this implies resaving the file under a new name and this is not wanted for the named pages. 
	So, we work around that and we ensure that the stash is created after the playground is created.
	This code has to be revisited."
	self title notNil ifTrue: [ self ensureStashPersistency ].
	^ result