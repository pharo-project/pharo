I can detect changes in the class structure.
My subclasses provides three blocks:

builderAccessor: a block to access the fill in the builder. 
classAccessor : a block to access in the old class.

By default I have a  comparer. This is a block to compare the values from the old class and the builder.
The subclasses can provide another comparer if they need to. 