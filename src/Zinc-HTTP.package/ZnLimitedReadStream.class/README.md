I am ZnLimitedReadStream, wrapping another read stream delegating to it 
but limiting reading to a fixed number of elements.

I can be atEnd while my wrapped stream is not.

My contentSpecies can be forced to be ByteArray (binary) or ByteString (ascii).

I do byte/char conversions on the fly (support bivalent access).

This implementation is really a kludge.

Part of Zinc HTTP Components.