responses
unicode: request
	"Answer a Unicode Test HTML page"
	
	^ ZnResponse ok: (ZnEntity html: self generateUnicodeTest)