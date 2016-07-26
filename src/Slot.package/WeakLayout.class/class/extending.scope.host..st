instance creation
extending: superLayout scope: aScope host: aClass
	^ (superLayout extendWeak: aScope)
		host: aClass;
		yourself