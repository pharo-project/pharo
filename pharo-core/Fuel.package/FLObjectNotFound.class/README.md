I represent an error which may happen during serialization, when trying to encode on the stream a reference to an object that should be encoded before, but it is not.

This usually happens when the graph changes during serialization. 

Another possible cause is a bug in the analysis step of serialization.