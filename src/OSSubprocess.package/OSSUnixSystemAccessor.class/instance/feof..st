files
feof: aCFile
	"Answers whether a FILE* is at the end of the file or not. This is trustful for pipes too"
	| result |
	result := self primitiveFeof: aCFile.
	^ result ~= 0
	
