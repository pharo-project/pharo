Extension { #name : #FileReference }

{ #category : #'*STON-Core' }
FileReference >> stonOn: stonWriter [
	self fileSystem isDiskFileSystem
		ifTrue: [ | diskFilePath |
			"in order to get $/ as delimiter and $. as working directory on all platforms"
			diskFilePath := path isWorkingDirectory
				ifTrue: [ '.' ]
				ifFalse: [ path pathString ].
			stonWriter 
				writeObject: self 
				named: STONFileReference stonName 
				listSingleton: diskFilePath ]
		ifFalse: [ 
			super stonOn: stonWriter ]
]
