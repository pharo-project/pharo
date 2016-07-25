options
logServerErrorDetails
	"Return whether we log server error details including a short strack trace, the default is true."
	
	^ self optionAt: #debugMode ifAbsent: [ true ] 