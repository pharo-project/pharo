*Graphics-Files
fromFileNamed: fileName 
	"Read a Form or ColorForm from the given file."
	
	^ FileStream 
			readOnlyFileNamed: fileName 
			do: [:aFile |
					aFile binary.
					self fromBinaryStream: aFile.]
		