TO USE LATER WITHOUT OSPROCESS
setOSFilePointerFromByteArray: externalFilePointer to: aDestByteArray offset: anOffsetNumber
	1 to: 4 do: [ :index | aDestByteArray at: (index + anOffsetNumber) put: (externalFilePointer at: index) ]
	