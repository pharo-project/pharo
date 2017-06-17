accessing
organizationChange
	^ organizationChange ifNil: [ self analyseChanges. organizationChange ]
