initialization
initialize
	super initialize.
	"setting custom brick themer for the whole spotter"
	self themer: self themer spotterThemer themer.
	
	self initializeListeners.
	self themer spotterThemer spotterStyleFor: self.
	
	self breadcrumbBrick: self newBreadcrumbBrick.
	self paneBrick: self newPaneBrick.

	
	
	