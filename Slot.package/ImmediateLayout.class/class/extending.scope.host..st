instance creation
extending: superLayout scope: aScope host: aClass
	^ superLayout extendImmediate
		host: aClass;
		yourself