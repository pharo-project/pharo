tests
tearDown

	fileToZip ensureDelete.
	zip close.	
	subdir ensureDeleteAll.
	zipFile ensureDelete.