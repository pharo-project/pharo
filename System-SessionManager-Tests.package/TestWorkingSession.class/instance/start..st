startup - shutdown
start: isImageStarting
	"do not modify vm parameters, external objects"
	self runStartup: isImageStarting.