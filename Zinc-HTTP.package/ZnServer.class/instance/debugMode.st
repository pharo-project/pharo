options
debugMode
	"Return whether we are in debug mode, the default is false."
	
	^ self optionAt: #debugMode ifAbsent: [ false ] 