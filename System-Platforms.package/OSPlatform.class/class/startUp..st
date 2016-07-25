system startup
startUp: isImageStarting
	"Determine the current platform."
	
	isImageStarting ifFalse: [  ^ self ].
	
	Current := self determineActivePlatform new.
	Current startUp: isImageStarting