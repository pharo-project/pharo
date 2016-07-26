Object is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass of Object) and its subclasses.

Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here.

Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes.

Class Variables:
	DependentsFields		an IdentityDictionary
		Provides a virtual 'dependents' field so that any object may have one
		or more dependent views, synchronized by the changed:/update: protocol.
		Note that class Model has a real slot for its dependents, and overrides
		the associated protocol with more efficient implementations.

Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as isMorph.





Miscellanous Discussions.

About at: index accepting float and not only integers
 
This behavior is also free in the sense that the failure code is only invoked when the
primitive fails and so adds nothing to the cost of successful accesses,
which are the high dynamic frequency operation.  It will also show up under
profiling if one is concerned about efficiency, and so isn't a hidden cost.

It is also in keeping with Smalltalk's mixed mode/arbitrary precision
implicit coercion number system that one *can* use fractions or floats as
indices.  Stripping out coercions like this will make the system more brittle.  So 
please do *not* remove this "hack".  I think it's a feature and a useful  one.

Can you give me an example that demonstrates the usefulness of this
feature?

| a r |
a := Array new: 10 withAll: 0.
r := Random new.
100 timesRepeat: [| v | v := r next * 10 + 1. a at: v put: (a at: v) + 1].
a

i.e. I didn't have to provide an explicit rounding step.  That's useful.  But in general anywhere 
where an index is derived by some calculation not having to provide the rounding step could be 
useful/helpful/more concise.  e.g. (n roundTo: 0.1) * 10 vs ((n roundTo: 0.1) * 10) asInteger.

Some thought went into the original choice.  It is not a hack but there by intent.  The integers are 
simply a subset of the reals and forcing the programmer to use them is favouring the machine 
above the programmer.

But I think you should justify getting rid of it rather than my having to justify keeping it.  Getting 
rid of it risks breaking code.  If it is there but does not harm then why get rid of it?

best Eliot Miranda 

