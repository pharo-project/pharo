checking
ensureDevelopmentVersionIsBaseline
	| devVersion |
	devVersion := workflow developmentVersion.
	(devVersion blessing == #baseline) 
		ifFalse: [ self createDevelopmentVersion ]