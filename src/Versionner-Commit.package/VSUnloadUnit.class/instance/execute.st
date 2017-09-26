operating
execute 
	| developmentVersion project |
	
	developmentVersion := self configurationClass project version: #development. 
	project := MTProject
		newFromVersion: developmentVersion
		inConfiguration: self configurationClass.
		
	(self configurationClass class selectors includes: #preUnload)
		ifTrue: [ self configurationClass perform: #preUnload ].
		
	project packages do: [ :eachPackage | 
		(MCWorkingCopy allManagers 
			detect: [ :each | each packageName = eachPackage name ])
			unload ].
	
	