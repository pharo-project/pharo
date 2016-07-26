mc related utils
allCurrentPackages
	"ScriptLoader new allCurrentPackages" 
	
	| copies |
	copies := self allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ].
	^ copies