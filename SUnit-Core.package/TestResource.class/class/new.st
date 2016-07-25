instance creation
new
	"Use #current to get the valid current instance.  Use of #new to get an instance (that should never be the current one) could be done in bizarre circumstances, so is not blocked, but will usually be inappropriate."
	
	^super new initialize.