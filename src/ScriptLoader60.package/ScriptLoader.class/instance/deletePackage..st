mc public utils
deletePackage: aString
	| toRemove toUnregister | 
	
	toRemove := (MCWorkingCopy allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ]) detect: [:each | each package name = aString].
	toRemove unload. 
	"With RPackage, each 'sub-package' is considered a separate package so we need to also unregister them"
	toUnregister := RPackageOrganizer default packages select: [:each | (each name = aString) or:  [each name beginsWith: aString,'-']].
	RPackageOrganizer default unregisterPackageNamed: aString.
