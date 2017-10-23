instance creation
on: aDomainObject

	^ self basicNew
		setModelBeforeInitialization: aDomainObject;
		initialize;
		yourself