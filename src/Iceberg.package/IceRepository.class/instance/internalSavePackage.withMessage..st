private
internalSavePackage: package withMessage: commitMessage
	"Save a package to the underlying store but do not commit"
	| workingCopy newVersion |
	workingCopy := 	package workingCopy.

	"This hack is copied from MCWorkingCopy code, I do not know why it is necessary."
	workingCopy modified: true; modified: false.
	
	(workingCopy repositoryGroup includes: self backend) 
		ifFalse: [ workingCopy repositoryGroup addRepository: self backend ].

	newVersion := MCVersion new
		setPackage: workingCopy package
		info: (IceMCVersionInfo package: package message: commitMessage)
		snapshot: workingCopy package snapshot
		dependencies: #(). "Note we do not track Monticello dependencies."

	self backend internalStoreVersion: newVersion.

	SystemAnnouncer uniqueInstance
		announce: (MCVersionSaved version: newVersion repository: self backend).
	
	^ newVersion 
