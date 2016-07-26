visiting
visitMCPackage: aPackage
	"visit a Metacello Package"

	| package version |
	
	package := MTPackage new.
	package name: aPackage name.
	
	version := MTVersion fromVersionName: aPackage file.
	package version: version.
	
	package dependencies: aPackage requires asOrderedCollection.
	
	project addDependency: package.
	^ package.
