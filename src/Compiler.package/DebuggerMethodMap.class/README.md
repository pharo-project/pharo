I am a place-holder for information needed by the Debugger to inspect method activations.  I insulate the debugger from details of code generation such as exact bytecode offsets and temporary variable locations. 

My function is to abstract the source map away from actual bytecode pcs to abstract bytecode pcs.

To reduce compilation time I try and defer as much computation to access time as possible as instances of me will be created after each compilation.

I maintain a WeakIdentityDictionary of method to DebuggerMethodMap to cache maps.  I refer to my method through a WeakArray to keep the map cache functional. If the reference from a DebuggerMethodMap to its method were strong then the method would never be dropped from the cache because the reference from its map would keep it alive.