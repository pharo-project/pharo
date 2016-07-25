private
changePadOfBits: bits width: width height: height depth: depth from: oldPad to: newPad 
	"Change padding size of bits."
	| srcRowByteSize dstRowByteSize newBits srcRowBase rowEndOffset |
	(#(8 16 32 ) includes: oldPad) ifFalse: [ ^ self error: 'Invalid pad: ' , oldPad printString ].
	(#(8 16 32 ) includes: newPad) ifFalse: [ ^ self error: 'Invalid pad: ' , newPad printString ].
	srcRowByteSize := (width * depth + oldPad - 1) // oldPad * (oldPad / 8).
	srcRowByteSize * height = bits size ifFalse: [ ^ self error: 'Incorrect bitmap array size.' ].
	dstRowByteSize := (width * depth + newPad - 1) // newPad * (newPad / 8).
	newBits := ByteArray new: dstRowByteSize * height.
	srcRowBase := 1.
	rowEndOffset := dstRowByteSize - 1.
	1 
		to: newBits size
		by: dstRowByteSize
		do: 
			[ :dstRowBase | 
			newBits 
				replaceFrom: dstRowBase
				to: dstRowBase + rowEndOffset
				with: bits
				startingAt: srcRowBase.
			srcRowBase := srcRowBase + srcRowByteSize ].
	^ newBits