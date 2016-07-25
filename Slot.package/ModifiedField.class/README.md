I encapsulate the modification of a slot.
The two use-cases are: slot renaming or a changed slot type.
Both cases enforce recompilation of all the methods accessing the corresponding variable.

This is unlike the ShiftedField modification which is only used when the type of the slot does not change.