accessing
forceNotBold
	"anything other than bold (700) is not changed.
	we only remove boldness that can be put back with 
	a TextAttribute bold."
	
	self weightValue = 700 
		ifTrue:[weightValue := 400].