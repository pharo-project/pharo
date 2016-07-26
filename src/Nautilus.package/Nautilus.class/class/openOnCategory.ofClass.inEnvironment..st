opening
openOnCategory: aCategory ofClass: aClass inEnvironment: anEnvironment
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance showInstance: aClass isMeta not.
	instance
		package: aClass package
		class: aClass
		protocol: aCategory.
	instance open.
	^ instance