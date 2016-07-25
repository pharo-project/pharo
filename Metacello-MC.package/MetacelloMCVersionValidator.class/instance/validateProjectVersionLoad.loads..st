loading
validateProjectVersionLoad: versionString loads: loadList
	| issues project currentVersion version |
	(issues := (self validateProjectVersion: versionString) select: [ :issue | issue isError ]) notEmpty
		ifTrue: [ ^ issues ].
	project := self configurationClass project.
	version := project
		version: versionString
		ifAbsent: [ 
			self
				recordValidationError: 'Version ' , versionString printString , ' does not exist.'
				callSite: #validateProjectVersionLoad:loads:
				reasonCode: #cannotResolveVersion.
			^ self validationReport ].
	version blessing = #broken
		ifTrue: [ self error: 'The specified version is #broken' ].
	self validateCleanLoadAndTestsForVersion: version loads: loadList.
	^ self validationReport