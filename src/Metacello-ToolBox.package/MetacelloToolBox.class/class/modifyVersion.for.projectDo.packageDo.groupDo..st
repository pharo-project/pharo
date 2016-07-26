scripts
modifyVersion: sourceVersionString for: configurationBasename projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock
	"Modify the project, package and/or group specifications for the given version"

	<apiDocumentation>
	self
		modifyVersion: sourceVersionString
		for: configurationBasename
		versionSpecsDo: [ :attribute :versionSpec | 
			versionSpec
				specListProjectDo: [ :spec | projectBlock value: attribute value: versionSpec value: spec ]
				packageDo: [ :spec | packageBlock value: attribute value: versionSpec value: spec ]
				groupDo: [ :spec | groupBlock value: attribute value: versionSpec value: spec ].
			true ]