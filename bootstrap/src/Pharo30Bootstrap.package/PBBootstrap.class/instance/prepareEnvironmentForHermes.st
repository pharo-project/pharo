preparation
prepareEnvironmentForHermes
	ringEnvironment := self originRepository asRing2EnvironmentWith: self allPackagesForHermes.
	ringEnvironment fixProtoObjectClassSuperclass.
	ringEnvironment addGlobalsNamed: #(Smalltalk Transcript FileStream MacRomanTextConverter ChangeSet Character SourceFiles Processor Display Sensor UTF8TextConverter SystemOrganization Undeclared TextConstants).
	ringEnvironment clean