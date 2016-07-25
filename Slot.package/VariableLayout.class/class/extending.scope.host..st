instance creation
extending: superLayout scope: aScope host: aClass
	^ (superLayout extendVariable: aScope)
		host: aClass;
		yourself