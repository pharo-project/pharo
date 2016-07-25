system startup
startUp: isImageStarting

	"only handle when lauching a new image"
	isImageStarting ifFalse: [ ^ self ].
	EndianCache := nil.