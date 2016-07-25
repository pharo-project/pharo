*Monticello-RPackage
mcWorkingCopy
	
	^ MCWorkingCopy allManagers 
		detect: [ :each | each package name = name ]
		ifNone: [ nil ].
	