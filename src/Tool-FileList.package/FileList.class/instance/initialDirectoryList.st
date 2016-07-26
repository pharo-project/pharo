initialization
initialDirectoryList

	| dirList |
	dirList := self fileSystem root directories collect: [ :each |
		FileDirectoryWrapper with: each name: each basename model: self].
	
	dirList isEmpty ifTrue:[
		dirList := Array with: (FileDirectoryWrapper 
			with: self fileSystem workingDirectory
			name: self fileSystem workingDirectory basename 
			model: self)].
		
	^dirList