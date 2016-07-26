actions
addPackage
	"Add a package as internal dependency."
	| searchUI packageWC package |
	searchUI := SearchFacade 
		mcPackageSearchRejectAll:  [ :workingCopy || versionedPackages pkgName |
			versionedPackages := project packages collect: [ :each | each name ].
			pkgName := workingCopy package name.			
			(versionedPackages includes: pkgName) or: [pkgName beginsWith: 'ConfigurationOf'] ]
		withInitialFilter: project name.
	packageWC := searchUI chooseFromOwner: self window.

	packageWC
		ifNotNil: [ 
			package := MTPackage newNamed: packageWC package name.
			owner project addDependency: package.
			packagesList roots: project packages.
				"package ancestry ancestors isEmpty
				ifTrue: [ 
					(MBPlatform current
						confirm:
							'The package: ' , package package name printString , ' has not been committed yet. Would you like to commit it into the '
							, repository description printString, ' repository?')
						ifTrue: [ 
							package repositoryGroup repositories size <= 1
								ifTrue: [ package repositoryGroup addRepository: repository ].
							package currentVersionInfo ] ] " ]