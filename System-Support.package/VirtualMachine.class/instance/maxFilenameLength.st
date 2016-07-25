accessing
maxFilenameLength
	"Return the maximal filename length (only under MacOS) as a string
	 or nil for other platforms "
	"Smalltalk vm maxFilenameLength"
	^ (self getSystemAttribute: 1201) ifNotNil: [ :string | string asNumber ]