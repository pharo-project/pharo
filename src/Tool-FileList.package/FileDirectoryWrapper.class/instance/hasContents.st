accessing
hasContents
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local) 
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents
	"
	hasContents ifNil: [
		hasContents := [ 
				item isDirectory 
				and: [ item isReadable 
				and: [ item hasDirectories ]]
			] on: Error do: [ :error | false ]].
	^ hasContents