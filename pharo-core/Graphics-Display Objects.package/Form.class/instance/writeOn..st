filein/out
writeOn: file
	"Write the receiver on the file in the format
		depth, extent, offset, bits."
	self writeAttributesOn: file.
	self writeBitsOn: file