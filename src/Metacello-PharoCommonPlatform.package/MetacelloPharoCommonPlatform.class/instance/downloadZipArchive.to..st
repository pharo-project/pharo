github support
downloadZipArchive: url to: outputFileName
	"download zip archive from <url> into <outputFileName>"

	| archive zipfile errorFileName |
	errorFileName := self
		downloadBasicFile: url
		to: outputFileName
		username: nil
		pass: nil.
	archive := ZipArchive new.
	zipfile := self fileHandleOn: outputFileName.
	zipfile containingDirectory
		readOnlyFileNamed: zipfile localName
		do: [ :fileStream | 
			[ archive readFrom: fileStream ]
				on: Error
				do: [ :ex | 
					self
						error:
							'Error during download (' , ex description , ') please check the file '
								, errorFileName printString , ' for error message.' ] ].
	self deleteFileNamed: errorFileName.
	^ archive