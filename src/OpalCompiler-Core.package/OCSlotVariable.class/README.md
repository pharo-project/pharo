I am a slot in a class. 

Actual code generation is forwarded to the Slot class.

e.g. an InstanceVariableSlot will generate bytecode for reading and writing an ivar, while other Slots either just call the reflective operations of the Slot (#read: and #write:to) or do their own code generation.