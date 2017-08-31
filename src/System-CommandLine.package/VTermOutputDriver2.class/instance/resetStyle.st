reset
resetStyle
	"Reset the style by creating a new VTermOutputStyle and reseting the terminal to default config"

	style := VTermOutputStyle new.
	installedStyle := style copy.
	self set: ResetStyle