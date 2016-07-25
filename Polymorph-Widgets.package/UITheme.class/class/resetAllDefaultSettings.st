accessing
resetAllDefaultSettings
	"Reset the default settings of all themes"
	"self resetAllDefaultSettings"
	self withAllSubclassesDo: [:cls | cls defaultSettings: nil]