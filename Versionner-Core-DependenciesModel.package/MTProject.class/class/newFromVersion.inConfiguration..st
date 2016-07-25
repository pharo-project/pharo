instance creation
newFromVersion: aMetacelloVersion inConfiguration: aConfigurationClass
	^ MCModel2MTModelVisitor new
		visitConfiguration: aConfigurationClass
		withVersion: aMetacelloVersion