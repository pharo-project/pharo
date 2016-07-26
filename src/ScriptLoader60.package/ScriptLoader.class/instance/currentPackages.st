mc related utils
currentPackages
	"self new currentPackages" 
	
	
	| copies |
	copies := self allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ].
	^ copies reject: [:each |
		self packagesNotToSavePatternNames
			anySatisfy: [:p | each packageName matchesRegex: p ] ].
	