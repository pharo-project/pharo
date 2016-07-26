pixel copies
copyPixelsGray: y 
	"Handle non-interlaced grayscale color mode (colorType = 0)"

	| base bits bytesLeft word |
	bitsPerChannel = 16 ifTrue: [
		"Warning: This is extremely slow. Besides we are downsampling to 8 bits!"
		| blitter |
		blitter := BitBlt bitPokerToForm: form.
		0 to: width - 1 do: [ :x |
			| high low value |
			high := thisScanline at: x * 2 + 1.
			low := thisScanline at: x * 2 + 2.
			value := (high * 256 + low = transparentPixelValue)
				ifTrue: [0 "transparent"]
				ifFalse: [high max: 1].
			blitter pixelAt: x @ y put: value ].
			^self ].

	"Just copy the bits"

	"This Smalltalk version might be easier to understand than the others below."
	base := y * (form width * bitsPerChannel + 31 // 32) + 1.
	bits := form bits.
	0 to: thisScanline size // 4 - 1 do: [ :i |
		| ii |
		ii := i * 4.
		"This somewhat weird mixture of (#* and #+) with (#bitShift: and #bitOr:) 
		is to make use of faster arithmetic bytecodes, but not of slow largeintegers."
		word :=
			(((thisScanline at: ii + 1) * 256 + 
			(thisScanline at: ii + 2) * 256 + 
			(thisScanline at: ii + 3)) bitShift: 8) bitOr: 
			(thisScanline at: ii + 4).
		bits at: base + i put: word ].	
	(bytesLeft := thisScanline size bitAnd: 3) = 0 ifFalse: [
		word := 0.
		thisScanline size - bytesLeft + 1 to: thisScanline size do: [ :ii |
			word := word * 256 + (thisScanline at: ii) ].
		word := word bitShift: 8 * (4 - bytesLeft).
		bits at: base + (thisScanline size // 4) put: word ].

	"This interesting technique (By Andreas Raab) is faster for very large images, but might be slower for small ones"
	"^self copyPixelsGrayWeirdBitBltHack: y ".
	"It uses the following method:
	PNGReadWriter >> copyPixelsGrayWeirdBitBltHack: y 
	""Handle non-interlaced black and white color mode (colorType = 0)
	By Andreas Raab""
	
	| source dest cmap |
	source := Form extent: 1 @ (thisScanline size // 4) depth: 32 bits: thisScanline.
	dest := Form extent: 1 @ (form bits size) depth: 32 bits: form bits.
	cmap := Smalltalk isLittleEndian
		ifTrue:[ColorMap 
					shifts: #(-24 -8 8 24) 
					masks: #(16rFF000000 16r00FF0000 16r0000FF00 16r000000FF)].
	(BitBlt toForm: dest)
		sourceForm: source;
		destX: 0 destY: (y * form width*bitsPerChannel//32) width: 1 height: (form width+31*bitsPerChannel//32);
		colorMap: cmap;
		combinationRule: 3;
		copyBits."
		
	"This interesting technique  (By Yoshiki Ohshima) is faster for very large images, but might be slower for small ones"
	"form bits copyFromByteArray2: thisScanline to: y * (form width* bitsPerChannel // 32)".
	"It uses the following method:
	BitMap >> copyFromByteArray2: byteArray to: i
	""This method should work with either byte orderings""

	| myHack byteHack |
	myHack := Form new hackBits: self.
	byteHack := Form new hackBits: byteArray.
	Smalltalk  isLittleEndian ifTrue: [byteHack swapEndianness].
	byteHack displayOn: myHack at:  0@i"