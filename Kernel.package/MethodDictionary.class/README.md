I'm a special dictionary holding methods. I am just like a normal Dictionary, except that I am implemented differently.  Each Class has an instance of MethodDictionary to hold the correspondence between selectors (names of methods) and methods themselves.

In a normal Dictionary, the instance variable 'array' holds an array of Associations.  Since there are thousands of methods in the system, these Associations waste space.  

Each MethodDictionary is a variable object, with the list of keys (selector Symbols) in the variable part of the instance.  The variable 'array' holds the values, which are CompiledMethods.

About flushCache methods and usage.

The VM caches method lookups in a lookup cache from class,selector pairs to method,primitive pairs, where primitive may be null.  This is called the first-level method lookup cache.  The JIT VM caches message lookups in machine code, so that a particular piece of machine code exists in a state that invokes a method for a specific class very fast by embedding the class reference in a register load and the target method in a call instruction, and having the target method verify this "cache probe" (this is an "in-line cache).  The JIT also caches the translation of a byte coded method to machine code, by hiding a reference to a machine code method in a byte coded method.

These caches can be invalidated in several circumstances:

1a. if one adds or removes a method from a class's method dictionary it may change the correct results of a lookup not merely of the class whose dictionary was updated but also subclasses of the class.
1b. if one replaces a method in a method dictionary this changes the target method for a lookup of the selector for the class and subclasses

2. if one wants to rewrite the byte code or literals of a method, for example because a Slot definition has changed, then if the method has been compiled to machine code, the machine code must be discarded before the new code may be executed

1a & 1b are done via Symbol>>flushCache.  In response the normal VM flushes its first-level method lookup cache, and the JIT also scans all of machine code looking for inline caches with that selector, and voiding them, reverting each send site for that selector to the "unlinked" state.

There used to be confusion in Squeak, which Pharo inherited, that using CompiledMethod>>flushCache was somehow the right way to void caches when updating method dictionaries, flushing the old method in the dictionary, if any, and the new method.  It isn't, precisely because adding or removing methods affects the visibility of inherited methods with the same selector.  So MethodDictionary code should use Symbol>>flushCache, and only once, on each update of a method dictionary.  As a result, the VM will ensure that the necessary send caches are flushed for that selector.

2. is done via CompiledMethod>>flushCache.  In response the VM searches the first-level method lookup cache and removes all entries whose target is the method.  In addition the JIT discards the machine code for the method, and searches for all send sites with that method's machine code as the target and voids them, reverting them to the unlinked state.

The VM must be told to flush the cached state for a compiled method via CompiledMethod>>flushCache and will /try/ and void the state for that method.  But it can't always deal with existing activations of that method, because if there are activations running the machine code, that machine code can't merely be thrown away, and can't be replaced because its length may change, depending on literals or byte codes.  So this kind of byte coded method manipulation needs to be done with case and some understanding of the total system state.