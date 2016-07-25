execute
execute
	| answer |
	answer := self uiManager confirm: 'Are you sure you want to load using ', target packageName.
	answer ifTrue:  [ | version |
		version := target configurationInfo currentVersion.
		version load: version defaultPackageNamesToLoad, { target packageName } ]