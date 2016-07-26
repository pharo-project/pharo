error handling
expected: aString 
	"Notify a problem at token 'here'."

	^ self notify: aString , ' expected' at: hereMark + requestorOffset