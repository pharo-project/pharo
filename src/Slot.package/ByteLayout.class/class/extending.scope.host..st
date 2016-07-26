instance creation
extending: superLayout scope: aScope host: aClass
	^ superLayout extendByte
		host: aClass;
		yourself