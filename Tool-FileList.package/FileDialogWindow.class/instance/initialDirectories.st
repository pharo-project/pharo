actions
initialDirectories
	"Answer the initial directories."
	
	| dirList |
	dirList := FileSystem disk root directories collect: [ :each |
		FileDirectoryWrapper with: each name: each basename model: self ].
	
	dirList isEmpty ifTrue: [ | workingDirectory 	wrapper |
		workingDirectory := FileSystem disk workingDirectory.
		wrapper := FileDirectoryWrapper 
			with:  workingDirectory
			name: workingDirectory basename
			model: self.
		dirList := Array with: wrapper ].
	^dirList