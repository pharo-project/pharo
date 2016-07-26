This class represents an external function called from Smalltalk. Instances of ExternalFunction can be created if the address/parameters of the function are known by some other means than loading from a shared library or compiling the appropriate primitive specification.

Instance variables:
	flags	<Integer>	a set of flags encoding the calling convention
	args	<Array of: ExternalType>		the parameters of the function

Implementation notes:

The arguments consist of an array with the first element defining the return type, the remaining arguments defining the parameters of the call.
