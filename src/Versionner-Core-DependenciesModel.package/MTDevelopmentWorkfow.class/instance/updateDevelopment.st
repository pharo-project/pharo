protocol
updateDevelopment
	"Update development baseline with information present in the project."

	| baseline versionString |
	versionString := self developmentVersion versionString.
	self isDevelopmentUsedInRelease 
		ifTrue: [ self createNextDevelopment: self nextDevelopmentVersionString ]
		ifFalse: [ baseline := MetacelloToolBox compiledMethodForVersion: self developmentVersion.
				baseline methodClass class removeSelector: baseline selector.
				self createBaseline: versionString ].
	self notifyConfigurationModified