system startup
startUp: resuming
	"The system is coming up. If it is on a new platform, clear out the existing handles."
	ExternalAddress startUp: resuming. "Make sure handles are invalid"
	resuming ifTrue:[self installSubclasses].
