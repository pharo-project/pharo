I am a hidden instance variable.

I work just like any other ivar, but I am not shown when asking for the #slots of a class.

The idea is that virtual slots can use hidden non-virtual slots to store their state. Example are PropertySlot, BooleanSlot...