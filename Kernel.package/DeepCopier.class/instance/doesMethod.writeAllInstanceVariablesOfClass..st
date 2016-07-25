checking
doesMethod: aSelector writeAllInstanceVariablesOfClass: aClass

	"This method returns true if the method of the given selector writes all instance variables of the given class.
	As an optimization, this method only checks that the last instance variable is written, assuming that all the rest are being written if the last one is."
 
	| lastFieldIndex method hasNoInstanceVariables |
	hasNoInstanceVariables := aClass instSize = 0.
	
	lastFieldIndex := aClass instSize.
	method := aClass compiledMethodAt: aSelector. 
	^ hasNoInstanceVariables or: [ method writesField: lastFieldIndex ]