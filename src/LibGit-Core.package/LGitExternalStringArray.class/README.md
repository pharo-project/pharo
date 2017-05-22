I represent an external array of strings. Since strings are of variable size, they can not be easily represented by the general implementation.
I take care of converting regular strings to external ones (which allocates and reserves the memory) and reading strings from memory.

I override #free to ensure that the memory for the external strings is correctly freed.