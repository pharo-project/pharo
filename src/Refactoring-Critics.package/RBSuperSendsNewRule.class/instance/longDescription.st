accessing
longDescription 
	^' This rule checks for method that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.
For example, a warning is raised when the statment self new initialize is found in a method.'