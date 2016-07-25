opening
openOnClass: aClass inEnvironment: anEnvironment
	| instance |
	instance := self new.
	instance
		browsedEnvironment: anEnvironment;
		showInstance: aClass isMeta not;
		package: (self parentOfClass: aClass)
			class: aClass;
		open.
	instance ui
		useLastPackagePatternStringForClass: aClass;
		showHierarchy: false;
		giveFocusToProtocol.
	^ instance