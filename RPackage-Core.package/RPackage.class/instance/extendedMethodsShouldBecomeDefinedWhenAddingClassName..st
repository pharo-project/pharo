private
extendedMethodsShouldBecomeDefinedWhenAddingClassName: aClassName
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"
	
	self extendedMethodsBecomeDefinedForClass: aClassName.
	self extendedMethodsBecomeDefinedForMetaclass: aClassName.
	