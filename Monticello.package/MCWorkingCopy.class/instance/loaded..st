operations
loaded: aVersion
	ancestry := MCWorkingAncestry new addAncestor: aVersion info.
	requiredPackages := OrderedCollection withAll: (aVersion dependencies collect: [:ea | ea package]).
	self modified: false.
	self changed