private
changesForSymbolicVersion: symbolicVersion workingCopy: wc
	| repository info ancestorVersion ref gofer resolved version packageSpec packageName ancestorVersionInfo |
	(repository := self selectRepositoryFromWorkingCopy: wc) ifNil: [ ^ self ].
	info := target configurationInfo.
	version := info project version: symbolicVersion.
	packageSpec := version
		packageNamed: wc packageName
		ifAbsent: [ ^ self uiManager inform: 'No package to compare ', target name, ' to in version' , symbolicVersion printString ].
	packageName := packageSpec file.
	ref := ((gofer := Gofer new)
		repository: repository;
		version: packageName) references first.
	(resolved := ref resolveAllWith: gofer) isEmpty
		ifTrue: [ 
			ref := ((gofer := Gofer new)
				repository: repository;
				package: packageName) references first.
			resolved := ref resolveAllWith: gofer ].
	ancestorVersionInfo := repository versionInfoFromVersionNamed: resolved last name.
	ancestorVersion := repository versionWithInfo: ancestorVersionInfo.
	self
		findChangesBetween: wc package snapshot
		toLabel: wc ancestry ancestorString , ' (' , info currentVersion versionString , ')'
		and: ancestorVersion snapshot
		fromLabel: ancestorVersion info name , ' (' , symbolicVersion printString , ' )'