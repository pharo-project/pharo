testing
hasChanged
	^ self changes notEmpty or: [ self organizationChange notNil ]