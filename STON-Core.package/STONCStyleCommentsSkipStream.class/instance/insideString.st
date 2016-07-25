private
insideString
	"Return true when we are currently inside a string where comments should be ignored."
	
	^ (delimiter = $') | (delimiter = $")