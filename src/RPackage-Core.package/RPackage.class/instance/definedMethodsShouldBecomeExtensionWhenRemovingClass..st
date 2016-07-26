private
definedMethodsShouldBecomeExtensionWhenRemovingClass: aClassName
	"the package may contain defined methods and their class is removed from the receiver. The status of such methods should now be extensions"
	
	self definedMethodsBecomeExtendedForClass: aClassName.
	self definedMethodsBecomeExtendedForMetaclass: aClassName.