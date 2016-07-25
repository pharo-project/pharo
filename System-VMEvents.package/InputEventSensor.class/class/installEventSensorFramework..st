class initialization
installEventSensorFramework: fetcherClass
	"Installs the new sensor framework."

	| newSensor |
	"Do some extra cleanup"
	SessionManager default unregisterClassNamed: #InputEventFetcher.
	InputEventFetcher deinstall.

	newSensor := self new.
	fetcherClass install.
	newSensor registerIn: InputEventFetcher default.
	
	"Shut down old sensor"
	Sensor shutDown.
	SessionManager default unregisterClassNamed: Sensor class name.

	"Note: We must use #become: here to replace all references to the old sensor with the new one, since Sensor is referenced from all the existing controllers."
	Sensor becomeForward: newSensor. "done"

	"Register the interrupt handler"
	UserInterruptHandler new registerIn: InputEventFetcher default.

	SessionManager default 
		registerGuiClassNamed: Sensor class name 
		atPriority: 20.
	"Project spawnNewProcessAndTerminateOld: true"