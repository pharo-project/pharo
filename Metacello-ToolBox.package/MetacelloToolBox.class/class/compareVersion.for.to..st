scripts
compareVersion: fromVersionString for: configurationBasename to: toVersionString
	"Return a MetacelloMCVersionDiffReport that whose fields 
		#additions
		#removals
		#modifications
	   refer to a dictionary whose keys are package names and whose 
	   values are a 2 element array. The first element is the mcz file 
	   name for the first version and the second element is the mcz file 
	   name for second version.
	There are also fields for the name of the configuration (#configuration), 
	the #from version string and the #to version string.
	
	The printString of the version report should provide all of the necessary info for 
	a developer."

	<apiDocumentation>
	| toolbox report |
	toolbox := self configurationNamed: configurationBasename.
	report := (toolbox project version: fromVersionString) difference: (toolbox project version: toVersionString).
	report 
		from: fromVersionString;
		to: toVersionString;
		configuration: configurationBasename.
	^ report