error handling
notify: aString 
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset