class initialization
initialize
	"initialize will only be sent to the defining class, not its subclasses"
	self current class addHacks.
	self updateConfigurationVersion