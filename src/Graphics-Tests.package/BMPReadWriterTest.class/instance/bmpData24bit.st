data
bmpData24bit
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest24.bmp') binary)
				contents
	"
	^(Base64MimeConverter mimeDecodeToBytes:
'Qk32AAAAAAAAADYAAAAoAAAACAAAAAgAAAABABgAAAAAAAAAAADEDgAAxA4AAAAAAAAAAAAA
AP8AAP8AAP8AAP8A/wAA/wAA/wAA/wAAAP8AAP8AAP8AAP8A/wAA/wAA/wAA/wAAAP8AAP8A
/////////////////wAA/wAAAP8AAP8A/////////////////wAA/wAAAAAAAAAA////////
////////AAD/AAD/AAAAAAAA////////////////AAD/AAD/AAAAAAAAAAAAAAAAAAD/AAD/
AAD/AAD/AAAAAAAAAAAAAAAAAAD/AAD/AAD/AAD/' readStream) contents