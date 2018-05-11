"
SUnit tests for class BMPReadWriter
"
Class {
	#name : #BMPReadWriterTest,
	#superclass : #TestCase,
	#category : #'Graphics-Tests-Files'
}

{ #category : #data }
BMPReadWriterTest >> bmpData16bit [
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16b.bmp') binary)
				contents
	"
	^ 'Qk24AAAAAAAAADYAAAAoAAAACAAAAAgAAAABABAAAAAAAIIAAADDDgAAww4AAAAAAAAAAAAA
4APgA+AD4AMfAB8AHwAfAOAD4APgA+ADHwAfAB8AHwDgA+AD/3//f/9//38fAB8A4APgA/9/
/3//f/9/HwAfAAAAAAD/f/9//3//fwB8AHwAAAAA/3//f/9//38AfAB8AAAAAAAAAAAAfAB8
AHwAfAAAAAAAAAAAAHwAfAB8AHwAAA==' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpData24bit [
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest24.bmp') binary)
				contents
	"
	^ 'Qk32AAAAAAAAADYAAAAoAAAACAAAAAgAAAABABgAAAAAAAAAAADEDgAAxA4AAAAAAAAAAAAA
AP8AAP8AAP8AAP8A/wAA/wAA/wAA/wAAAP8AAP8AAP8AAP8A/wAA/wAA/wAA/wAAAP8AAP8A
/////////////////wAA/wAAAP8AAP8A/////////////////wAA/wAAAAAAAAAA////////
////////AAD/AAD/AAAAAAAA////////////////AAD/AAD/AAAAAAAAAAAAAAAAAAD/AAD/
AAD/AAD/AAAAAAAAAAAAAAAAAAD/AAD/AAD/AAD/' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpData32bit [
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest32b.bmp') binary)
				contents
	"
	^ 'Qk04AQAAAAAAADYAAAAoAAAACAAAAAgAAAABACAAAAAAAAIBAADDDgAAww4AAAAAAAAAAAAA
AP8AAAD/AAAA/wAAAP8AAP8AAAD/AAAA/wAAAP8AAAAA/wAAAP8AAAD/AAAA/wAA/wAAAP8A
AAD/AAAA/wAAAAD/AAAA/wAA////AP///wD///8A////AP8AAAD/AAAAAP8AAAD/AAD///8A
////AP///wD///8A/wAAAP8AAAAAAAAAAAAAAP///wD///8A////AP///wAAAP8AAAD/AAAA
AAAAAAAA////AP///wD///8A////AAAA/wAAAP8AAAAAAAAAAAAAAAAAAAAAAAAA/wAAAP8A
AAD/AAAA/wAAAAAAAAAAAAAAAAAAAAAAAAD/AAAA/wAAAP8AAAD/AAAA' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpData4bit [
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest4.bmp') binary)
				contents
	"
	^ 'Qk12BAAAAAAAADYEAAAoAAAACAAAAAgAAAABAAgAAAAAAEAAAADEDgAAxA4AAAAAAAAAAAAA
AAAAAAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAMDAwADA3MAA8MqmAAAgQAAAIGAAACCAAAAg
oAAAIMAAACDgAABAAAAAQCAAAEBAAABAYAAAQIAAAECgAABAwAAAQOAAAGAAAABgIAAAYEAA
AGBgAABggAAAYKAAAGDAAABg4AAAgAAAAIAgAACAQAAAgGAAAICAAACAoAAAgMAAAIDgAACg
AAAAoCAAAKBAAACgYAAAoIAAAKCgAACgwAAAoOAAAMAAAADAIAAAwEAAAMBgAADAgAAAwKAA
AMDAAADA4AAA4AAAAOAgAADgQAAA4GAAAOCAAADgoAAA4MAAAODgAEAAAABAACAAQABAAEAA
YABAAIAAQACgAEAAwABAAOAAQCAAAEAgIABAIEAAQCBgAEAggABAIKAAQCDAAEAg4ABAQAAA
QEAgAEBAQABAQGAAQECAAEBAoABAQMAAQEDgAEBgAABAYCAAQGBAAEBgYABAYIAAQGCgAEBg
wABAYOAAQIAAAECAIABAgEAAQIBgAECAgABAgKAAQIDAAECA4ABAoAAAQKAgAECgQABAoGAA
QKCAAECgoABAoMAAQKDgAEDAAABAwCAAQMBAAEDAYABAwIAAQMCgAEDAwABAwOAAQOAAAEDg
IABA4EAAQOBgAEDggABA4KAAQODAAEDg4ACAAAAAgAAgAIAAQACAAGAAgACAAIAAoACAAMAA
gADgAIAgAACAICAAgCBAAIAgYACAIIAAgCCgAIAgwACAIOAAgEAAAIBAIACAQEAAgEBgAIBA
gACAQKAAgEDAAIBA4ACAYAAAgGAgAIBgQACAYGAAgGCAAIBgoACAYMAAgGDgAICAAACAgCAA
gIBAAICAYACAgIAAgICgAICAwACAgOAAgKAAAICgIACAoEAAgKBgAICggACAoKAAgKDAAICg
4ACAwAAAgMAgAIDAQACAwGAAgMCAAIDAoACAwMAAgMDgAIDgAACA4CAAgOBAAIDgYACA4IAA
gOCgAIDgwACA4OAAwAAAAMAAIADAAEAAwABgAMAAgADAAKAAwADAAMAA4ADAIAAAwCAgAMAg
QADAIGAAwCCAAMAgoADAIMAAwCDgAMBAAADAQCAAwEBAAMBAYADAQIAAwECgAMBAwADAQOAA
wGAAAMBgIADAYEAAwGBgAMBggADAYKAAwGDAAMBg4ADAgAAAwIAgAMCAQADAgGAAwICAAMCA
oADAgMAAwIDgAMCgAADAoCAAwKBAAMCgYADAoIAAwKCgAMCgwADAoOAAwMAAAMDAIADAwEAA
wMBgAMDAgADAwKAA8Pv/AKSgoACAgIAAAAD/AAD/AAAA//8A/wAAAP8A/wD//wAA////APr6
+vr8/Pz8+vr6+vz8/Pz6+v/////8/Pr6//////z8AAD/////+fkAAP/////5+QAAAAD5+fn5
AAAAAPn5+fk=' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpData8bit [
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest8.bmp') binary)
				contents
	"
	^ 'Qk12BAAAAAAAADYEAAAoAAAACAAAAAgAAAABAAgAAAAAAEAAAADEDgAAxA4AAAAAAAAAAAAA
AAAAAAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAMDAwADA3MAA8MqmAAAgQAAAIGAAACCAAAAg
oAAAIMAAACDgAABAAAAAQCAAAEBAAABAYAAAQIAAAECgAABAwAAAQOAAAGAAAABgIAAAYEAA
AGBgAABggAAAYKAAAGDAAABg4AAAgAAAAIAgAACAQAAAgGAAAICAAACAoAAAgMAAAIDgAACg
AAAAoCAAAKBAAACgYAAAoIAAAKCgAACgwAAAoOAAAMAAAADAIAAAwEAAAMBgAADAgAAAwKAA
AMDAAADA4AAA4AAAAOAgAADgQAAA4GAAAOCAAADgoAAA4MAAAODgAEAAAABAACAAQABAAEAA
YABAAIAAQACgAEAAwABAAOAAQCAAAEAgIABAIEAAQCBgAEAggABAIKAAQCDAAEAg4ABAQAAA
QEAgAEBAQABAQGAAQECAAEBAoABAQMAAQEDgAEBgAABAYCAAQGBAAEBgYABAYIAAQGCgAEBg
wABAYOAAQIAAAECAIABAgEAAQIBgAECAgABAgKAAQIDAAECA4ABAoAAAQKAgAECgQABAoGAA
QKCAAECgoABAoMAAQKDgAEDAAABAwCAAQMBAAEDAYABAwIAAQMCgAEDAwABAwOAAQOAAAEDg
IABA4EAAQOBgAEDggABA4KAAQODAAEDg4ACAAAAAgAAgAIAAQACAAGAAgACAAIAAoACAAMAA
gADgAIAgAACAICAAgCBAAIAgYACAIIAAgCCgAIAgwACAIOAAgEAAAIBAIACAQEAAgEBgAIBA
gACAQKAAgEDAAIBA4ACAYAAAgGAgAIBgQACAYGAAgGCAAIBgoACAYMAAgGDgAICAAACAgCAA
gIBAAICAYACAgIAAgICgAICAwACAgOAAgKAAAICgIACAoEAAgKBgAICggACAoKAAgKDAAICg
4ACAwAAAgMAgAIDAQACAwGAAgMCAAIDAoACAwMAAgMDgAIDgAACA4CAAgOBAAIDgYACA4IAA
gOCgAIDgwACA4OAAwAAAAMAAIADAAEAAwABgAMAAgADAAKAAwADAAMAA4ADAIAAAwCAgAMAg
QADAIGAAwCCAAMAgoADAIMAAwCDgAMBAAADAQCAAwEBAAMBAYADAQIAAwECgAMBAwADAQOAA
wGAAAMBgIADAYEAAwGBgAMBggADAYKAAwGDAAMBg4ADAgAAAwIAgAMCAQADAgGAAwICAAMCA
oADAgMAAwIDgAMCgAADAoCAAwKBAAMCgYADAoIAAwKCgAMCgwADAoOAAwMAAAMDAIADAwEAA
wMBgAMDAgADAwKAA8Pv/AKSgoACAgIAAAAD/AAD/AAAA//8A/wAAAP8A/wD//wAA////APr6
+vr8/Pz8+vr6+vz8/Pz6+v/////8/Pr6//////z8AAD/////+fkAAP/////5+QAAAAD5+fn5
AAAAAPn5+fk=' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpDataR5G6B5 [
	"This is a BMP file based on BitmapV4Header which is currently unsupported."
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16-R5G6B5.bmp') binary)
				contents
	"
	^ 'Qk3IAAAAAAAAAEYAAAA4AAAACAAAAAgAAAABABAAAwAAAIIAAADDDgAAww4AAAAAAAAAAAAA
APgAAOAHAAAfAAAAAAAAAOAH4AfgB+AHHwAfAB8AHwDgB+AH4AfgBx8AHwAfAB8A4AfgB///
////////HwAfAOAH4Af//////////x8AHwAAAAAA//////////8A+AD4AAAAAP//////////
APgA+AAAAAAAAAAAAPgA+AD4APgAAAAAAAAAAAD4APgA+AD4AAA=' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpDataX4R4G4B4 [
	"This is a BMP file based on BitmapV4Header which is currently unsupported."
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest16-X4R4G4B4.bmp') binary)
				contents
	"
	^ 'Qk3IAAAAAAAAAEYAAAA4AAAACAAAAAgAAAABABAAAwAAAIIAAADDDgAAww4AAAAAAAAAAAAA
AA8AAPAAAAAPAAAAAAAAAPAA8ADwAPAADwAPAA8ADwDwAPAA8ADwAA8ADwAPAA8A8ADwAP8P
/w//D/8PDwAPAPAA8AD/D/8P/w//Dw8ADwAAAAAA/w//D/8P/w8ADwAPAAAAAP8P/w//D/8P
AA8ADwAAAAAAAAAAAA8ADwAPAA8AAAAAAAAAAAAPAA8ADwAPAAA=' base64Decoded
]

{ #category : #data }
BMPReadWriterTest >> bmpDataX8R8G8B8 [
	"This is a BMP file based on BitmapV4Header which is currently unsupported."
	"Created via:
		(Base64MimeConverter mimeEncode: 
			(FileStream readOnlyFileNamed: 'bmptest32-X8R8G8B8.bmp') binary)
				contents
	"
	^ 'Qk1IAQAAAAAAAEYAAAA4AAAACAAAAAgAAAABACAAAwAAAAIBAADDDgAAww4AAAAAAAAAAAAA
AAAA/wAA/wAA/wAAAAAAAAAA/wAAAP8AAAD/AAAA/wAA/wAAAP8AAAD/AAAA/wAAAAD/AAAA
/wAAAP8AAAD/AAD/AAAA/wAAAP8AAAD/AAAAAP8AAAD/AAD///8A////AP///wD///8A/wAA
AP8AAAAA/wAAAP8AAP///wD///8A////AP///wD/AAAA/wAAAAAAAAAAAAAA////AP///wD/
//8A////AAAA/wAAAP8AAAAAAAAAAAD///8A////AP///wD///8AAAD/AAAA/wAAAAAAAAAA
AAAAAAAAAAAAAAD/AAAA/wAAAP8AAAD/AAAAAAAAAAAAAAAAAAAAAAAAAP8AAAD/AAAA/wAA
AP8AAA==' base64Decoded
]

{ #category : #'tests - reading' }
BMPReadWriterTest >> testBmp16Bit [
	| reader form |
	reader := BMPReadWriter new on: self bmpData16bit readStream.
	form := reader nextImage.
	"special black here to compensate for zero-is-transparent effect"
	self assert: (form colorAt: 7 @ 1) = Color red.
	self assert: (form colorAt: 1 @ 7) = Color green.
	self assert: (form colorAt: 7 @ 7) = Color blue.
	self assert: (form colorAt: 4 @ 4) = Color white.
	self assert: (form pixelValueAt: 1 @ 1) = 32768
]

{ #category : #'tests - reading' }
BMPReadWriterTest >> testBmp24Bit [
	| reader form |
	reader := BMPReadWriter new on: (ReadStream on: self bmpData24bit).
	form := reader nextImage.
	self assert: (form colorAt: 7@1) = Color red.
	self assert: (form colorAt: 1@7) = Color green.
	self assert: (form colorAt: 7@7) = Color blue.
	self assert: (form colorAt: 4@4) = Color white.
	self assert: (form pixelValueAt: 1@1) = 16rFF000001.

]

{ #category : #'tests - reading' }
BMPReadWriterTest >> testBmp32Bit [
	| reader form |
	reader := BMPReadWriter new on: self bmpData32bit readStream.
	form := reader nextImage.
	self assert: (form colorAt: 7 @ 1) = Color red.
	self assert: (form colorAt: 1 @ 7) = Color green.
	self assert: (form colorAt: 7 @ 7) = Color blue.
	self assert: (form colorAt: 4 @ 4) = Color white.
	self assert: (form pixelValueAt: 1 @ 1) = 4278190080
]

{ #category : #'tests - reading' }
BMPReadWriterTest >> testBmp4Bit [
	| reader form |
	reader := BMPReadWriter new on: self bmpData4bit readStream.
	form := reader nextImage.
	self assert: (form colorAt: 1 @ 1) = Color black.
	self assert: (form colorAt: 7 @ 1) = Color red.
	self assert: (form colorAt: 1 @ 7) = Color green.
	self assert: (form colorAt: 7 @ 7) = Color blue.
	self assert: (form colorAt: 4 @ 4) = Color white
]

{ #category : #'tests - reading' }
BMPReadWriterTest >> testBmp8Bit [
	| reader form |
	reader := BMPReadWriter new on: self bmpData8bit readStream.
	form := reader nextImage.
	self assert: (form colorAt: 1 @ 1) = Color black.
	self assert: (form colorAt: 7 @ 1) = Color red.
	self assert: (form colorAt: 1 @ 7) = Color green.
	self assert: (form colorAt: 7 @ 7) = Color blue.
	self assert: (form colorAt: 4 @ 4) = Color white
]
