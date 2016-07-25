data
bmpData16bit
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16b.bmp') binary)
				contents
	"
	^(Base64MimeConverter mimeDecodeToBytes:
'Qk24AAAAAAAAADYAAAAoAAAACAAAAAgAAAABABAAAAAAAIIAAADDDgAAww4AAAAAAAAAAAAA
4APgA+AD4AMfAB8AHwAfAOAD4APgA+ADHwAfAB8AHwDgA+AD/3//f/9//38fAB8A4APgA/9/
/3//f/9/HwAfAAAAAAD/f/9//3//fwB8AHwAAAAA/3//f/9//38AfAB8AAAAAAAAAAAAfAB8
AHwAfAAAAAAAAAAAAHwAfAB8AHwAAA==' readStream) contents