instance creation
owner: anOwningPresenter on: aDomainObject

	^ self basicNew
		owner: anOwningPresenter;
		setModelBeforeInitialization: aDomainObject;
		yourself