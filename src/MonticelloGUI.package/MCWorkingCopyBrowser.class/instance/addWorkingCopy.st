actions
addWorkingCopy
	| name |
	
	name := UIManager default request: 'Name of package:'.
	name isEmptyOrNil ifFalse: [
		RPackageOrganizer default registerPackageNamed: name.		
		workingCopy := MCWorkingCopy forPackage: (MCPackage new name: name).
		workingCopyWrapper := MCDependentsWrapper with: workingCopy model: self.
		self repositorySelection: 0 ].
	
	self 
		workingCopyListChanged; 
		changed: #workingCopySelection; 
		repositoryListChanged.
	self changedButtons.