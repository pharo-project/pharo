private-encoding
writeHeader
	| byte |
	stream position = 0 ifTrue: 
		[ "For first image only"
		self nextPutAll: 'GIF89a' asByteArray.
		self writeWord: width.	"Screen Width"
		self writeWord: height.	"Screen Height"
		byte := 128.	"has color map"
		byte := byte bitOr: (bitsPerPixel - 1 bitShift: 5).	"color resolution"
		byte := byte bitOr: bitsPerPixel - 1.	"bits per pixel"
		self nextPut: byte.
		self nextPut: 0.	"background color."
		self nextPut: 0.	"reserved"
		colorPalette do: 
			[ :pixelValue | 
			self
				nextPut: ((pixelValue bitShift: -16) bitAnd: 255);
				nextPut: ((pixelValue bitShift: -8) bitAnd: 255);
				nextPut: (pixelValue bitAnd: 255) ].
		loopCount notNil ifTrue: 
			[ "Write a Netscape loop chunk"
			self nextPut: Extension.
			self nextPutAll: #(
					255
					11
					78
					69
					84
					83
					67
					65
					80
					69
					50
					46
					48
					3
					1
				) asByteArray.
			self writeWord: loopCount.
			self nextPut: 0 ] ].
	delay notNil | transparentIndex notNil ifTrue: 
		[ self
			nextPut: Extension;
			nextPutAll: #(249 4 ) asByteArray;
			nextPut: (transparentIndex 
					ifNil: [ 0 ]
					ifNotNil: [ 9 ]);
			writeWord: (delay ifNil: [ 0 ]);
			nextPut: (transparentIndex ifNil: [ 0 ]);
			nextPut: 0 ].
	self nextPut: ImageSeparator.
	self writeWord: 0.	"Image Left"
	self writeWord: 0.	"Image Top"
	self writeWord: width.	"Image Width"
	self writeWord: height.	"Image Height"
	byte := interlace 
		ifTrue: [ 64 ]
		ifFalse: [ 0 ].
	self nextPut: byte