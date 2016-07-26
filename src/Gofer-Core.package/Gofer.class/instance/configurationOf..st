references
configurationOf: aProjectName
	"Add a package reference to the configuration of the given project"
	references addLast: (GoferConfigurationReference name: aProjectName)