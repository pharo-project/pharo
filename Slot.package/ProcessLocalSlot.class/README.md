A ProcessLocal is a slot that references different objects in different processes.

Internally it is implemented with a ProcessLocalVariable. Writing into the slot writes a field in the current process. 

myProcessLocalVariable value: anObject

Reading into the Slot reads the only field in the array.

myProcessLocalVariable value.

This slot inherits from IndexedSlot to guarantee that this slot has a real field inside an object.

This slot should be used wisely:

1) The value is stored weakly, an example of this would be:
obj := MyClass new.
obj local: Object new.
obj local. “anObject” <—— Now we see the object
Smalltalk garbageCollect.
obj local. “nil” <—— Now we don’t

2) This variable are stored in a WeakArray in the Process, so massive use of them will make the array in the processes start growing a lot and often.
