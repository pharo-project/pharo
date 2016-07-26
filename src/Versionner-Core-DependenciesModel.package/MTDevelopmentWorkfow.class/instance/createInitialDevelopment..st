private
createInitialDevelopment: versionString
	"Create an initial development version with an empty configuration and return the configuration class."

	| toolbox |
	toolbox := MetacelloToolBox configurationNamed: project name.	
		
	"Only a static method available on the toolbox to create baselines"
	MetacelloToolBox
		createBaseline: versionString 
		for: toolbox project configuration class name
		repository: project repository
		requiredProjects: #()
		packages: #()
		repositories: #()
		dependencies: #()
		groups: #().
		
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: versionString;
		commitMethod.
		
	^toolbox project configuration class