opening
openOnPackage: aPackage inEnvironment: anEnvironment

	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance selectedPackage: aPackage.
	instance open.
	instance ui showHierarchy: false.
	^ instance