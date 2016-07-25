accessing
centralDirectoryHeaderSize

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment convertToSystemString.
	systemCdExtraField := cdExtraField.
	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size
