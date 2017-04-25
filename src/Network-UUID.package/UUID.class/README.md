I am UUID.
I am a ByteArray.

An RFC4122 Universally Unique Identifier (UUID) is an opaque 128-bit number that can be used for identification purposes. Concretely, a UUID is a 16 element byte array.

The intent of UUIDs is to enable distributed systems to uniquely identify information without significant central coordination. In this context the word unique should be taken to mean "practically unique" rather than "guaranteed unique".

Usage:

	UUID new.
	
Whenever you create a new UUID, the shared, default UUIDGenerator will be used to generate a new, unique UUID.

See UUIDGenerator for extensive documentation on how UUIDs are actually generated.

UUIDs have a standard string representation, like this:

	3ccb64f1-aa04-0d00-bbbc-259a0f871399
	
The representation consists of 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens).

My #printOn: #printString and #asString methods produce this representation.  My class' #fromString or instance #readFrom: parse it.

Alternatively, my base 36 number representation is  the shortest representation still being able to work as filenames etc since it does not depend on case nor characters that might cause problems, and it is reasonably short.

See #asString36 and my class' #fromString36:

UUIDs are basically opaque (contain no interesting content) and should be treated as indivisable values. Do not use parts of them, since these will most probably no longer be unique.

See also:

  http://en.wikipedia.org/wiki/UUID
  https://tools.ietf.org/html/rfc4122