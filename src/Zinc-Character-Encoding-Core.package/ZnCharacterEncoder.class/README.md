I am ZnCharacterEncoder, I encode and decode Character objects to and from a binary stream.
I am an abstract class with following protocol:

#nextFromStream:
#nextPut:toStream:
#encodedByteCountFor:
#backOnStream:

The first two are compatible with TextConverter and subclasses.

I add some convenience methods:

#encodeString:
#decodeBytes:
#encodedByteCountForString:

Contrary to older encoders, I work strictly from strings to bytes and vice versa and I will throw errors instead of silently ignoring them.

I also implement optimized bulk operations:

#next:putAll:startingAt:toStream:
#readInto:startingAt:count:fromStream:

Part of Zinc HTTP Components.