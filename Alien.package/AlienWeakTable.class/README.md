This class supports simple post-mortem finalization of values associated with gc'ed objects.  An object to be finalized is registered in the table together with another object called 'the tag'. The finalizable object is held onto by the table weakly, the tag object--strongly. A table is initialized with the owner object, which is the object that performs the actual finalization. Some time after a finalizable object is garbage-collected, the owner is sent the #finalize: message with the object's tag as the argument.

Instance Variables:
	accessProtect <Semaphore>  - A mutex protecting state
	firstUnusedIndex <Integer> - The lowest index in strongArray that is empty (an invariant)
	lastUsedIndex <Integer> - The highest index in strongArray that is not empty (an invariant)
	weakArray <WeakArray> - The array of objects whose death we're interested in.
	strongArray <Array> - The array of corresponding objects that wll be passed to the owner when their corresponding element in weakArray is garbage collected.
	owner <Object> - The object that is sent finalize: with the tag of an object that has been garbage-collected.