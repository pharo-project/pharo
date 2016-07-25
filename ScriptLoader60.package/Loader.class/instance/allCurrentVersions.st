mc related utils
allCurrentVersions
	"self new allCurrentVersions"
	
	| copies names |
	copies := MCWorkingCopy allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ].
	names := copies collect:
		[:ea |  ea ancestry ancestorString ].
	^ names reject: #isEmpty