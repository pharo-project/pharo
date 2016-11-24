mc public utils
unregisterPackage: aString

	| toRemove | 
	toRemove := (MCWorkingCopy allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ]) detect: [:each | each package name = aString].
	MCWorkingCopy registry removeKey: toRemove package.