accessing
schemesNotUsingDoubleSlash
	"Most URL schemes use a double slash, as in http://
	but some don't, return a list of those"
	
	^ #( #mailto #telnet )