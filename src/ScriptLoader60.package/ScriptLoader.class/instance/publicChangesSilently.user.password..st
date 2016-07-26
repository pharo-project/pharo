ci
publicChangesSilently: changes user: username password: password
	self setToRepositoriesPassword: password to: username.
	self csForLastUpdateAndPatchUpdatesList: 'Update'.
	self copyPackagesFromWaitingFolderToHomeRepository