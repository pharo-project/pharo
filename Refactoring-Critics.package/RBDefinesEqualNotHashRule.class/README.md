This smell arises when a class defines #= also and not #hash. If #hash is not defined then the instances of the class might not be able to be used in sets since equal element must have the same hash.

When a class defines = also and not hash, this can lead to really subtle bugs and behavior where sometimes it appears that an object is in a set and sometimes not. 

One pattern proposed by Kent Beck in Best Smalltalk Practices is to define hash in terms of instance variable hash xor. Here is an example:
	
	Book>>= anotherBook
		^ (self author = anotherBook author) and: [self title = anotherBook title]
	
	Book>>hash
		^ (self title hash bitXor: self title hash		