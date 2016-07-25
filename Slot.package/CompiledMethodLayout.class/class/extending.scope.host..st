instance creation
extending: superLayout scope: aScope host: aClass
	^ superLayout extendCompiledMethod
		host: aClass;
		yourself