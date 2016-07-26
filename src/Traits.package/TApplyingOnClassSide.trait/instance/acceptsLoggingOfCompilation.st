compiling
acceptsLoggingOfCompilation
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself."

	^ self theNonMetaClass acceptsLoggingOfCompilation