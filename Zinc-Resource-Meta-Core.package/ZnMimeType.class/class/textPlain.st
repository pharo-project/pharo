convenience
textPlain
	"Note that the charset is set to utf8 automatically"

	^ (self main: 'text' sub: 'plain') 
		setCharSetUTF8; 
		yourself