convenience
textHtml
	"Note that the charset is set to utf8 automatically"
	
	^ (self main: 'text' sub: 'html') 
		setCharSetUTF8; 
		yourself