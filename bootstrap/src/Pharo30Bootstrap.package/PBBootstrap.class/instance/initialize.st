initialization
initialize
	"Initialize the logging"

	(CustomStringStreamBeacon with: FileStream stdout)
		startFor: PBStepStartedSignal , PBStepFinishedSignal