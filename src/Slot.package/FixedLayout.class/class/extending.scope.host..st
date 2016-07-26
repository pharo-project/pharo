instance creation
extending: superLayout scope: aScope host: aClass
	^ (superLayout extend: aScope)
		host: aClass;
		yourself