I am a helper class for handling the contents of Set.
My main purpose is to allow a Set to contain nil.
Since nil represents an empty slot in a Set, it cannot be stored directly in the array of values. Instead, an instance of SetElement is used as a wrapper. See implementors and senders of #asElement for wrapping, and #enclosedSetElement for unwrapping.