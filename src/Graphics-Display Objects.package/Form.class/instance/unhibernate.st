filein/out
unhibernate
	"If my bitmap has been compressed into a ByteArray,
	then expand it now, and return true."
	| resBits |
	bits isForm ifTrue:[
		resBits := bits.
		bits := Bitmap new: self bitsSize.
		resBits displayResourceFormOn: self.
		^true].
	bits == nil ifTrue:[bits := Bitmap new: self bitsSize. ^true].
	(bits isMemberOf: ByteArray)
		ifTrue: [bits := Bitmap decompressFromByteArray: bits. ^ true].
	^ false