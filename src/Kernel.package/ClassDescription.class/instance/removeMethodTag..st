accessing tags
removeMethodTag: aSymbol
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods.
	Here we could remove what we not need"
	self organization removeCategory: aSymbol