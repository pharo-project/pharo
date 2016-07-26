public
open: aResolvable writable: aBoolean 
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."
	
	| path |
	path := self resolve: aResolvable.
	^ store handleClass 
		open: (FileReference fileSystem: self path: path) 
		writable: aBoolean
		