initialization
initialize
	"ProcessBrowser initialize"
	SuspendedProcesses ifNil: [ SuspendedProcesses := IdentityDictionary new ].
	self registerWellKnownProcesses