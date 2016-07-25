accessing class hierarchy
methodPropertiesClass
	"Answer the class to use to create a method's properties, which can be a poor man's way
	 to add instance variables to subclassses of CompiledMethod.  Subclasses of CompiledMethod
	 should define a corresponding subclass of AdditionalMethodState that adds any instance variables
	 required, and override this method to answer that class."
	^AdditionalMethodState