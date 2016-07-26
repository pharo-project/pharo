actions
selectPathName: aString
	"Select the directory and set the file name text
	from the given string."

	| reference |
	reference := aString asFileReference asAbsolute.
	
	reference exists
		ifFalse: [ ^ self selectPathName: reference parent fullName ].
	
	reference isDirectory 
		ifTrue: [ ^ self selectDirectory: reference ].
		
	"must be a file then"
	self selectDirectory: reference parent.
	self fileNameText: reference basename.