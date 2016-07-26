I add a number of facilities to those in ClassDescription:
	A set of all my subclasses (defined in ClassDescription, but only used here and below)
	A name by which I can be found in a SystemDictionary
	A classPool for class variables shared between this class and its metaclass
	A list of sharedPools which probably should be supplanted by some better mechanism.

My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.

The slot 'subclasses' is a redundant structure.  It is never used during execution, but is used by the development system to simplify or speed certain operations.  