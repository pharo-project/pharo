mc public utils
unloadPackage: aString
	"self new unloadPackage: 'Sixx'"
	| toRemove | 
	toRemove := (self allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ]) detect: [:each | each package name = aString].
	toRemove unload.