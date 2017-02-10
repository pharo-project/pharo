I am UUID.
I am a ByteArray.

An RFC4122 Universally Unique Identifier (UUID) is an opaque 128-bit number that can be used for identification purposes. Concretely, a UUID is a 16 element byte array.

The intent of UUIDs is to enable distributed systems to uniquely identify information without significant central coordination. In this context the word unique should be taken to mean "practically unique" rather than "guaranteed unique".

Usage:

	UUID new.

See UUIDGenerator for extensive documentation on how UUIDs are actually generated.