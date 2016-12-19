I am UUIDGenerator, I generate UUIDs.

An RFC4122 Universally Unique Identifier (UUID) is an opaque 128-bit number that can be used for identification purposes. Concretely, a UUID is a 16 element byte array.

The intent of UUIDs is to enable distributed systems to uniquely identify information without significant central coordination. In this context the word unique should be taken to mean "practically unique" rather than "guaranteed unique".
 
I generate UUIDs similar, in spirit, to those defined in RFC4122, though I use version 0 to indicate that I follow none of the defined versions. This does not matter much, if at all, in practice.

I try to conform to the following aspects:
 - each 'node' (machine, image, instance) should generate unique UUIDs
 - even when generating UUIDs at a very fast rate, they should remain unique
 - be fast and efficient

To achieve this goal, I
- take several aspects into account to generate a unique node ID
- combine a clock, a counter and some random bits
- hold a state, protected for multi user access

I can generate about 500K UUIDs per second.

Implementation:

Although a UUID should be seen as totally opaque, here is the concrete way I generate one:
- the first 8 bytes are the microsecond clock value with the smallest quantity first; this means that the later of these 8 bytes will be identical when generated with small(er) timespans; within the same clock resolution interval, the full first 8 bytes will be identical
- the next 2 bytes represent a counter with safe overflow, held as protected state inside me; after 2*16 this value will repeat; the counter initalizes with a random value
- the next 2 bytes are simply random, based on the system PRNG, Random
- the final 4 bytes represent the node ID; the node ID is unique per instance of me, across OS environments where the image might run; the node ID is the MD5 hash of a string that is the concatenation of several elements (see #computeNodeIdentifier)
 
Some bits are set to some predefined value, to indicate the variant and version (see #setVariantAndVersion:)

Usage:

  UUIDGenerator next.
  UUIDGenerator current next.
  UUIDGenerator new next.

Sharing an instance is more efficient and correct.
Instances should be reset whenever the image comes up.

See also:

  http://en.wikipedia.org/wiki/UUID
  https://tools.ietf.org/html/rfc4122
