path primitives
lookupDirectory: fullPath filename: fileName

	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and return an array containing:

	<fileName> 			
	<creationTime> 			(in seconds since the start of the Smalltalk time epoch)
	<modificationTime> 	(in seconds since the start of the Smalltalk time epoch)
	<dirFlag> 				DirFlag is true if the entry is a directory
	<fileSize> 				FileSize the file size in bytes or zero for directories
	<posixPermissions> 	Numeric Notation 
	<symLinkFlag>			seemingly, symLinkFlag is true if the entry is a symLink

	On Unix, the empty path denotes '/'. 
   On Macs and PCs, it is the container of the system volumes."

 	<primitive: 'primitiveDirectoryEntry' module: 'FilePlugin'>
	
	^ #badDirectoryPath

