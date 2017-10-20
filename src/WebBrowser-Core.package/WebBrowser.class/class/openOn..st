instance-creation
openOn: aURLString
	"Open the webbrowser on the given URL"
	
	^self webbrowserClassForPlatform openOn: aURLString	
