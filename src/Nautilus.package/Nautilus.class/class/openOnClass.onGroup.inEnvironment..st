opening
openOnClass: aClass onGroup: aGroup inEnvironment: anEnvironment
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance showInstance: aClass isMeta not.
	instance selectedGroup: aGroup.
	instance
		package: aClass package
		class: aClass.
	instance open.
	^ instance