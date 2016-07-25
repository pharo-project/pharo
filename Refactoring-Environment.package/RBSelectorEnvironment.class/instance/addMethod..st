adding
addMethod: aMethod
	"this is a method to improve addition of methods, as class and selector data can be retrieved from a single compiled method"
	
	self addClass: aMethod methodClass selector: aMethod selector