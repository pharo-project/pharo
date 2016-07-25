constants
validSignatures
	"Return the valid signatures for a zip file"
	^Array 
		with: LocalFileHeaderSignature
		with: CentralDirectoryFileHeaderSignature
		with: EndOfCentralDirectorySignature