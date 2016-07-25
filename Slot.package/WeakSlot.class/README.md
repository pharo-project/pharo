A WeakSlot is a slot that references weakly an object.

Internally it is implemented with a weak array of size one. Writing into the slot writes the only field in the array. 

weakArray at: 1 put: aValue.

Reading into the Slot reads the only field in the array.

weakArray at: 1 .

This slot inherits from IndexedSlot to guarantee that this slot has a real field inside an object.