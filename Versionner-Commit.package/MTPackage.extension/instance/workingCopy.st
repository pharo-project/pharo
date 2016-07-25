*Versionner-Commit
workingCopy
	^ MCWorkingCopy allManagers 
		detect: [ :each | each packageName = self name ]
		ifNone: [ nil ]