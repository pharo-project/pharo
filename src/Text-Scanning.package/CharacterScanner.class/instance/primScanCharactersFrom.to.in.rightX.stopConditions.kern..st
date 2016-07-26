scanning
primScanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernDelta
	"Primitive. This is the inner loop of text display--but see 
	scanCharactersFrom: to:rightX: which would get the string, 
	stopConditions and displaying from the instance. March through source 
	String from startIndex to stopIndex. If any character is flagged with a 
	non-nil entry in stops, then return the corresponding value. Determine 
	width of each character from xTable, indexed by map. 
	If dextX would exceed rightX, then return stops at: 258. 
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257. Optional. 
	See Object documentation whatIsAPrimitive.
	Historical note: this primitive has been unusable since about Squeak 2.8 when the shape of the CharracterScanner class changed. It is left here as a reminder that the actual primitive still needs supporting in the VM to keep old images such as Scratch1.4 alive - tpr"
	<primitive: 103>
	^self basicScanByteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 