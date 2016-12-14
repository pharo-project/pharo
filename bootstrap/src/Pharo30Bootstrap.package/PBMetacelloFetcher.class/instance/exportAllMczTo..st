actions
exportAllMczTo: aDirectory
	
	| repository directory |
	directory := aDirectory asFileReference ensureCreateDirectory.
	repository := MCDirectoryRepository new directory: directory.
	self withMczDo: [ :mcz | repository basicStoreVersion: mcz ] group: 'full-', group.