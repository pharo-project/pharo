initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		initializeLayout;
		initializeAppearance;
		tasks: #();
		orderedTasks: OrderedCollection new