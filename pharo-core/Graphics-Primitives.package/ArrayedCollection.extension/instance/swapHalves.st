*Graphics-Primitives
swapHalves
 		"A normal switch in endianness (byte order in words) reverses the order of 4 bytes.  That is not correct for SoundBuffers, which use 2-bytes units.  If a normal switch has be done, this method corrects it further by swapping the two halves of the long word.
 	This method is only used for 16-bit quanities in SoundBuffer, ShortIntegerArray, etc."
 
 	| hack blt |
 	"The implementation is a hack, but fast for large ranges"
 	hack := Form new hackBits: self.
 	blt := (BitBlt toForm: hack) sourceForm: hack.
 	blt combinationRule: Form reverse.  "XOR"
 	blt sourceY: 0; destY: 0; height: self size; width: 2.
 	blt sourceX: 0; destX: 2; copyBits.  "Exchange bytes 0&1 with 2&3"
 	blt sourceX: 2; destX: 0; copyBits.
 	blt sourceX: 0; destX: 2; copyBits.
