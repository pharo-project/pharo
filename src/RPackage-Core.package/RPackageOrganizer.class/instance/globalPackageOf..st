package access from class
globalPackageOf: aClass
	"this method should return the 'global' parent package of aClass, that means the package holding the (possible) subcategory in which aClass is concretely defined. For example, 'Object package' returns Kernel-Object, whereas 'PackageOrganizer packageOf: Object' returns Kernel. So I guess that all use of 'packageOf' should be replaced by this method  "

	"RPackageOrganizer default globalPackageOf: Object"

	| classPackage |
	classPackage := self packageOf: aClass.
	^ self packages
		detect: [ :aRPackage | aRPackage ~= classPackage and: [ aRPackage systemCategories includes: classPackage name ] ]
		ifNone: [ classPackage ]