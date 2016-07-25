converting
asExternalPointer
	"Convert the receiver assuming that it describes a pointer to an object."
	^(ExternalAddress new)
		basicAt: 1 put: (self byteAt: startOffset + 1);
		basicAt: 2 put: (self byteAt: startOffset + 2);
		basicAt: 3 put: (self byteAt: startOffset + 3);
		basicAt: 4 put: (self byteAt: startOffset + 4);
	yourself