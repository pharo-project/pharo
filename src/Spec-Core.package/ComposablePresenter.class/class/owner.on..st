instance creation
owner: anOwningPresenter on: aDomainObject

	^ self basicNew
		owner: anOwningPresenter;
		on: aDomainObject;
		yourself