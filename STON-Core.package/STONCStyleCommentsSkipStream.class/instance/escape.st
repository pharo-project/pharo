private
escape
	"Return true when we previously read a backslash escape inside a string, 
	so that the next string delimiter should be returned as is"
	
	^ escape = true