instantiation
newWithWorkingCopy: anMBConfigurationInfo
	"Create a new dependent project with the given project."
	| requiredProject  version |
	
	requiredProject := self new
		name: anMBConfigurationInfo name configurationBaseName;
		configurationClass: anMBConfigurationInfo configurationClass;
		yourself.
	requiredProject displayName: requiredProject name.
	requiredProject editRepositories.
	version := (anMBConfigurationInfo hasVersion: #stable)
		ifTrue: [ #stable ]
		ifFalse: [ #bleedingEdge ].
	requiredProject version: (MTVersion fromVersionString: version).
	^ requiredProject