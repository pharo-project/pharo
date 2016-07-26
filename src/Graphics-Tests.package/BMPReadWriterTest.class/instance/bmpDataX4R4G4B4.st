data
bmpDataX4R4G4B4
	"This is a BMP file based on BitmapV4Header which is currently unsupported."
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16-X4R4G4B4.bmp') binary)
				contents
	"
	^(Base64MimeConverter mimeDecodeToBytes:
'Qk3IAAAAAAAAAEYAAAA4AAAACAAAAAgAAAABABAAAwAAAIIAAADDDgAAww4AAAAAAAAAAAAA
AA8AAPAAAAAPAAAAAAAAAPAA8ADwAPAADwAPAA8ADwDwAPAA8ADwAA8ADwAPAA8A8ADwAP8P
/w//D/8PDwAPAPAA8AD/D/8P/w//Dw8ADwAAAAAA/w//D/8P/w8ADwAPAAAAAP8P/w//D/8P
AA8ADwAAAAAAAAAAAA8ADwAPAA8AAAAAAAAAAAAPAA8ADwAPAAA='
readStream) contents