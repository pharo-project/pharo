system startup
resumeSystemSettings
	"Load persisted settings if the last settings were not loaded from this machine"
	| thisComputerID |
	
	thisComputerID := GlobalIdentifier uniqueInstance computerUUID.
	lastSettingsComputerID ~= thisComputerID ifTrue: [ 
		lastSettingsComputerID := thisComputerID.
		self updateSettingNodes ]