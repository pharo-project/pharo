operations
unload
	"Unloads mcpackage, rpackage, classes and method extensions from this working copy"
	MCPackageLoader unloadPackage: self package.		
	package packageSet unregister.
	self unregister.
