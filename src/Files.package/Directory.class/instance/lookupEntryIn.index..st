primitives
lookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path 
	(i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty 
	path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. 
	DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. 
	The primitive returns nil when index is past the end of the directory. It fails if the given path 
	is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin' error: errorCode >
	
	^ #badDirectoryPath

