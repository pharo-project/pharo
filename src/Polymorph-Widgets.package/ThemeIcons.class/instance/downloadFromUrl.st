loading
downloadFromUrl
	| dir |
	dir := self class destinationPath ensureCreateDirectory. 
	^ self downloadTo: dir