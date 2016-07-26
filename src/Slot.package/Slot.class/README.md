I'm a meta-object for accessing a slot in an Object. 

I define a protocol to read (#read:) and to write (#write:to:) values. 

For customizing a subclass can override the meta-object-protocol methods. See subclasses for examples.

Vocabulary:
- variable: named accessor for a Slot
- Slot: class-side meta-object, mapping of names to values using a MOP to fields
- field: space occupied in an object, used to hold values accessed via Slots