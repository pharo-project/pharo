accessing
currentInstallerOrError
	| installer |
	installer := EmbeddedFreeTypeFontInstaller readSlotNamed: #current.
	self assert: installer notNil.
	^ installer