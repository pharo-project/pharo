instance creation
fromBytes: aByteArray index: anInteger
	"share alike instances"
	| newFace |
	
	self allInstancesDo: [:inst |
		(inst fileContentsExternalMemoryBytes = aByteArray and: [inst index = anInteger])
			ifTrue: [^inst "validate"]].
	newFace := self basicNew
		fileContentsExternalMemory: (FreeTypeExternalMemory bytes: aByteArray);
		index: anInteger;
		yourself.
	self register: newFace.
	^ newFace