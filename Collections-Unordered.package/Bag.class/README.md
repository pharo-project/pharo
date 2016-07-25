I represent an unordered collection of possibly duplicate elements.
	
I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.