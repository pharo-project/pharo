data
bmpDataR5G6B5
	"This is a BMP file based on BitmapV4Header which is currently unsupported."
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16-R5G6B5.bmp') binary)
				contents
	"
	^(Base64MimeConverter mimeDecodeToBytes:
'Qk3IAAAAAAAAAEYAAAA4AAAACAAAAAgAAAABABAAAwAAAIIAAADDDgAAww4AAAAAAAAAAAAA
APgAAOAHAAAfAAAAAAAAAOAH4AfgB+AHHwAfAB8AHwDgB+AH4AfgBx8AHwAfAB8A4AfgB///
////////HwAfAOAH4Af//////////x8AHwAAAAAA//////////8A+AD4AAAAAP//////////
APgA+AAAAAAAAAAAAPgA+AD4APgAAAAAAAAAAAD4APgA+AD4AAA='
readStream) contents