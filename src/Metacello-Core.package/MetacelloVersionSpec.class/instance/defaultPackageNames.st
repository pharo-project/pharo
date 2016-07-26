loading
defaultPackageNames
	"if there is a package named 'default' (a group) then it defines the default package names,
	 otherwise answer a list of all of the package names in this version"

	self packages packageNamed: 'default' ifAbsent: [ ^self packageNames ].
	^#('default')