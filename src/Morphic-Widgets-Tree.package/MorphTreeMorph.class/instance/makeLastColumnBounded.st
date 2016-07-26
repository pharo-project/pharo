column handling
makeLastColumnBounded
	"Make the last column horizontally resizable with a resizer"
	self lastColumnBounded 
		ifFalse: [
			unboundLastColumn := false.
			self columnsChanged]
