instance creation
browseSendersOf: aSymbol 
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self browse: aSymbol senders asSendersOf: aSymbol