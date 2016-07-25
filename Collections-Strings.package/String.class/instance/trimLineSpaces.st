copying
trimLineSpaces
	"Trim the spaces from the right side of each line. Useful for code"
	
	^ self class streamContents: [ :str |
		self lines 
			do: [ :line | str nextPutAll: line trimRight]
			separatedBy: [str cr]]
		
