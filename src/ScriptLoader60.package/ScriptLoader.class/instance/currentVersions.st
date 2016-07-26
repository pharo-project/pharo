mc related utils
currentVersions
	"ScriptLoader new currentVersions"
	
	| copies |
	copies := self allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ].
	^ copies collect:
		[:ea |  ea ancestry ancestorString ]