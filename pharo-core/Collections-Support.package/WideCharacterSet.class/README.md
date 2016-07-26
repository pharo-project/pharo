WideCharacterSet is used to store a Set of WideCharacter with fast access and inclusion test.

Implementation should be efficient in memory if sets are sufficently sparse.

Wide Characters are at most 32bits.
We split them into 16 highBits and 16 lowBits.

map is a dictionary key: 16 highBits value: map of 16 lowBits.

Maps of lowBits  are stored as arrays of bits in a ByteArray.
If a bit is set to 1, this indicate that corresponding character is present.
8192 bytes are necessary in each lowmap.
Empty lowmap are removed from the map Dictionary.

A byteArrayMap is maintained in parallel with map for fast handling of ByteString.
(byteArrayMap at: i+1) = 0 means that character of asciiValue i is absent, = 1 means present.