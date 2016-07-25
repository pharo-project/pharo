serialize/materialize
serializeInstance: aCompiledMethodToSerialize with: anEncoder
	| header bytecodesPlusTrailerSize cmSize cmInitialPC aCompiledMethod |
	"In 64 bit VMs the header is actually 64 bits wide but only 32 bits
	contain information."
	aCompiledMethod := self class transformationForSerializing value: aCompiledMethodToSerialize.

	header := aCompiledMethod header.
	cmSize := aCompiledMethod size.
	cmInitialPC := aCompiledMethod initialPC.
	bytecodesPlusTrailerSize := cmSize - cmInitialPC + 1.

	anEncoder 
		encodeInt32: header; "Signed int as of Pharo 40"
		encodeUint16: bytecodesPlusTrailerSize.	

	cmInitialPC to: cmSize
		do: [ :index | anEncoder encodeByte: (aCompiledMethod at: index) ]