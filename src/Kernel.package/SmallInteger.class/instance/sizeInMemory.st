memory
sizeInMemory
    "SmallInteger occupy 0 bytes since the only space occupied by a SmallInteger is the space of the slot containing it.  There is no SmallInteger object beyond the slot. From another POV, it could be 4, the size of the slot. But we don't count the size of the slots, they are already counted in the containing object, that's why it should answer 0."
   ^0.