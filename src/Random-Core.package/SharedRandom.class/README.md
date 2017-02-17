I represent a Random number generator that can be shaded between different code.

I wrap the access to my parent's functionality (#next, #nextInt: and #next:into:)  through a mutex making me safe for multi threaded use.

To access the default shared random number generator, do:

	SharedRandom globalGenerator.
	
In principle it is better to use a shared generator since multiple users will create a more random pattern.