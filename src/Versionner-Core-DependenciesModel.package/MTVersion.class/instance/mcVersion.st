accessing
mcVersion
	"Get the MetacelloMCVersion object representing this version"

	| configurationClass |
	configurationClass := project configurationClass.
	configurationClass project version: versionString. 