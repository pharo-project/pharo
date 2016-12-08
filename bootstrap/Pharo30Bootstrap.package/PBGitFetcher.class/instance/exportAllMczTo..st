actions
exportAllMczTo: aDirectory
	
	| fileRepository directory |
"	self ensureRepository.
	directory := aDirectory asFileReference ensureCreateDirectory.
	fileRepository := MCDirectoryRepository new directory: directory.
	self withMczDo: [ :mcz | fileRepository basicStoreVersion: mcz ]."