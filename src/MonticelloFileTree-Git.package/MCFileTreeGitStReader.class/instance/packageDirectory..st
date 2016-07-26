accessing
packageDirectory: aDirectoryName
	"Set the package name at the same time, no need to read the metadata anymore."

	packageDirectory := aDirectoryName.
	package := MCPackage named: self packageNameFromPackageDirectory