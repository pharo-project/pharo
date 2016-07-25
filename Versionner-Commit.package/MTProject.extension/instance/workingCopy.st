*Versionner-Commit
workingCopy
	| packageName |
	packageName := self configurationClass package name.
	^ MCWorkingCopy allManagers detect: [ :each | each packageName = packageName ]