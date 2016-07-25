I'm an object holding the result of loading a file containing Pharo code definitions in chunk format.
I create ring definitions for the elements inside the chunk stream.
Heavily inspired from FilePackage.

| internalStream |
internalStream := (String new: 1000) writeStream.
SystemOrganization 
	fileOutCategory: 'Tool-ExternalBrowser'
	on: internalStream.
RingChunkImporter fromStream: internalStream contents readStream.