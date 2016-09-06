icon
packageIconFor: aPackage
	| mcpackage |
	
	PackagesIconsCache
		at: aPackage
		ifPresent: [:packageIcon |
			packageIcon class == IconicButtonStateHolder
				ifTrue: [
					"convert the IconicButtonStateHolder previously cached by
					this method before returning"
					^ packageIcon asIcon ].
			^ packageIcon ].

	aPackage classes isEmpty 
		ifTrue: [^ PackagesIconsCache at: aPackage put: (self iconNamed: #emptyPackageIcon) ].
	
	mcpackage := aPackage mcPackage.
	(mcpackage notNil and: [ mcpackage isDirty ])
		ifTrue: [ | icon |
			icon := IconicButton new 
						target: NautilusUI;
						actionSelector: #saveDirtyPackages:;
						arguments: {{ aPackage }};
						labelGraphic: (aPackage definedClasses
											ifEmpty: [ (self iconNamed: #dirtyMonticelloPackageIcon) ]
											ifNotEmpty: [ (self iconNamed: #dirtyPackageIcon) ]);
						color: Color transparent;
						extent: 15 @ 16;
						helpText: 'Save the package';
						borderWidth: 0.
			 PackagesIconsCache at: aPackage put: (IconicButtonStateHolder forNautilus: icon).
			^ icon ].

	(mcpackage notNil and: [(mcpackage name = aPackage name ) and: [ aPackage definedClasses isEmpty]])
		ifTrue: [ ^ PackagesIconsCache at: aPackage put: (self iconNamed: #monticelloPackageIcon) ].

	^ PackagesIconsCache at: aPackage put: (self iconNamed: #packageIcon)