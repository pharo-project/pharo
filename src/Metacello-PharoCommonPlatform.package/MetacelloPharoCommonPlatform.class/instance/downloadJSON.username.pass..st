github/bitbucket support
downloadJSON: url username: username pass: pass
	"return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

	| jsonFileName jsonFile errorFileName |
	jsonFileName := self
		tempFileFor: 'tags-' , self processPID
		suffix: 'json'.
	errorFileName := self
		downloadBasicFile: url
		to: jsonFileName
		username: username
		pass: pass.
	[ jsonFile := self fileHandleOn: jsonFileName.
	jsonFile containingDirectory
		readOnlyFileNamed: jsonFile localName
		do: [ :fileStream | 
			| result |
			result := (Smalltalk at: #MCFileTreeJsonParser)
				parseStream: fileStream self
				deleteFileNamed: errorFileName.
			^ result ] ]
		on: Error
		do: [ :ex | 
			self
				error:
					'Error during download (' , ex description , ') please check the file '
						, errorFileName printString , ' for error message.' ]