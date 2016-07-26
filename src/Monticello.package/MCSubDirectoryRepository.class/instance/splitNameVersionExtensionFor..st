i/o
splitNameVersionExtensionFor: aBaseName

	| file version |
	file := aBaseName asFileReference.
	version := (file extensions at: file extensions size - 1) asNumber.
	^ { file base. version. file extension }.
	
	
	