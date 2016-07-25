accessing-computed
packages
	"Return the list of dependent packages for the last baseline"

	| packageSpecs |
	packages ~~ nil
		ifTrue: [ ^ packages ].
	self versionForInfoList ifNil: [ ^ #() ].
	packages := OrderedCollection new.
	packageSpecs := self versionForInfoList packages.
	packageSpecs := packageSpecs asSortedCollection: [ :a :b | a name <= b name ].
	packages
		addAll:
			(packageSpecs
				collect: [ :pkgSpec | 
					| packageName |
					(packageName := pkgSpec getFile) ifNil: [ packageName := pkgSpec name asString ].
					(MBPackageInfo named: packageName packageName: pkgSpec name asString spec: pkgSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ packages