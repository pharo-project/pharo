instance creation
mapBitsFrom: srcBitMask to: dstBitMask 
	"Return an array consisting of the shift and the mask for
	mapping component values out of srcBitMask and into dstBitMask.
	While this computation is somewhat complicated it eases the batch
	conversion of all the pixels in BitBlt."
	| srcBits dstBits srcLow srcHigh dstLow dstHigh bits mask shift |
	(srcBitMask = 0 or: [ dstBitMask = 0 ]) ifTrue: 
		[ ^ #(0 0 ) ].	"Zero mask and shift"
	"Compute low and high bit position for source and dest bit mask"
	srcLow := srcBitMask lowBit - 1.
	srcHigh := srcBitMask highBit.
	dstLow := dstBitMask lowBit - 1.
	dstHigh := dstBitMask highBit.
	"Compute the number of bits in source and dest bit mask"
	srcBits := srcHigh - srcLow.
	dstBits := dstHigh - dstLow.
	"Compute the maximum number of bits we can transfer inbetween"
	bits := srcBits min: dstBits.
	"Compute the (unshifted) transfer mask"
	mask := (1 bitShift: bits) - 1.
	"Shift the transfer mask to the mask the highest n bits of srcBitMask"
	mask := mask bitShift: srcHigh - bits.
	"Compute the delta shift so that the most significant bit of the
	source bit mask falls on the most significant bit of the dest bit mask.
	Note that delta is used for #bitShift: so
		shift > 0 : shift right
		shift < 0 : shift left
	e.g., if dstHigh > srcHigh we need to shift left and if dstHigh < srcHigh
	we need to shift right. This leads to:"
	shift := dstHigh - srcHigh.
	"And that's all we need"
	^ Array 
		with: shift
		with: mask