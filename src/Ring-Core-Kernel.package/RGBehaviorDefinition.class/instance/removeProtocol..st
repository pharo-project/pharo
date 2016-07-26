adding/removing protocols
removeProtocol: aString
	"Removes a protocol named aString (if exists)"

	protocols remove: aString ifAbsent:[]