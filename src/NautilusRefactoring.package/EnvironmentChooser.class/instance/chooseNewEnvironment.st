actions
chooseNewEnvironment
	| packagesAndClasses packages classes packageEnvironment classEnvironment |
	
	packagesAndClasses := PackageAndClassChooser new chooseModalTo: self window.
	(packagesAndClasses isNil or: [ packagesAndClasses isEmpty])
		ifTrue: [ ^ nil ].
	
	self flag: 'TODO create proper environments in a less ugly way'.
	packages := OrderedCollection new.
	classes := OrderedCollection new.
	
	packagesAndClasses do: [ :each |
		(each isKindOf: RPackage)
			ifTrue: [ packages add: each ]
			ifFalse: [ classes add: each ]].
		
	packageEnvironment := RBPackageEnvironment packages: packages.
	classEnvironment := RBClassEnvironment classes: classes.
	
	classes ifEmpty: [ ^ packageEnvironment ].
	packages ifEmpty: [ ^ classEnvironment ].
	^ packageEnvironment | classEnvironment