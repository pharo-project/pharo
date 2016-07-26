actions
savePackage: packageName withMessage: commitMessage
	"Mostly copied from MCFileRepositoryInspector>>#saveChanges"
	| workingCopy newVersion |
	workingCopy := 	Iceberg workingCopyForPackageNamed: packageName.
	newVersion := workingCopy newVersionWithMessage: commitMessage in: self repository. 
	self repository storeVersion: newVersion.

	self refresh.
	self announcer announce: IceCommited new.
