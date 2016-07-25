I am a Slot that does not allocate one field for each slot. Instead, all PropertySlots of the whole hierarchy are allocated in a dictionary that is stored in an invisible slot (see PropertyBaseSlot)

Keep in mind:
- I am slower than instance variables
- there is the overhead of the Dictionary. Only if there are multiple ones of me in the class hierarchy you will save memory