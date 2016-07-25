Browser the current image:

	ExternalBrowser openOn: CurrentImage new.

Browse a FileOut

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	SystemOrganization 
		fileOutCategory: 'Tool-ExternalBrowser'
		on: internalStream.
	ExternalBrowser browseStream: internalStream contents readStream.