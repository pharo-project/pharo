options
signalProgress
	"Return true if I signal HTTPProgress notifications during execution."
	
	^ self optionAt: #signalProgress ifAbsent: [ false ]