I am responsible for encoding and decoding various kinds of compiled method trailer data.
I should not expose any binary data outside of myself, so all tools which working with compiled methods
should ask me to encode the meta-data, they want to be added to the compiled method trailer, as well as retrieve it.

To add a new kind of trailer, you should give it a proper name and define it in the #trailerKinds method at my class side.
Then you need to implement a corresponding #encode<your name> and #decode<your name> methods at instance side. Then add any public accessor methods, which will use a newly introduced trailer kind for communicating with outer layer(s).

An encodeXXX methods should store result (byte array) into encodedData instance variable.

A decodeXXX methods should read the data from compiled method instance, held by 'method' ivar,
and always set 'size' ivar (denoting a total length of trailer in compiled method) and optionally 'data' ivar which should keep a decoded data, ready to be used by outer layer(s) using accessor method(s) you providing.

The kind of compiled method trailer is determined by the last byte of compiled method.

The byte format used is following: 
	"2rkkkkkkdd"

where 'k' bits stands for 'kind' , allowing totally 64 different kinds of method trailer
and 'd' bits is data.

Following is the list of currently defined trailer kinds:

NoTrailer , k = 000000, dd unused
method has no trailer, and total trailer size bytes is always 1

ClearedTrailer, k = 000001, 
method has cleared trailer (it was set to something else, but then cleared) 
dd+1  determines the number of bytes for size field, and size is a total length of trailer bytes
So a total length of trailer is: 1 + (dd + 1) + size

TempsNamesQCompress, k = 000010
the trailer contains a list of method temp names,  compressed using qCompress: method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer.
So a total length of trailer is:  1 + (dd + 1) + size

TempsNamesZip, k = 000011
the trailer contains a list of method temp names,  compressed using GZIP compression method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
So a total length of trailer is: 1 + (dd + 1) + size

SourceBySelector, k = 000100
the trailer indicates , that method source is determined by a class + selector where it is installed to. 
Trailer size = 1.

SourceByStringIdentifier, k = 000101
the trailer indicates , that method source is determined by a class + some ByteString identifier. 
dd+1  determines the number of bytes for size of ByteString identifier, and size is number of bytes of string.
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceQCompress, k = 000110
the trailer contains an utf-8 encoded method source code, compressed using qCompress method
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed source code
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceZip, k = 000111
the trailer contains an utf-8 encoded method source code, comressed using GZIP 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
A total length of trailer is:  1 + (dd + 1) + size

VarLengthSourcePointer, k = 001000
the trailer is variable-length encoded source pointer. 
dd bits is unused.

ExtendedKind, k = 001001
the next byte of trailer (one that prepends the last byte of compiled method)
denotes an extended kind of trailer, allowing to use additional 256 kinds of encoding method's trailer in future. 

SourcePointer, k = 111111 
the trailer is encoded source pointer. Total trailer size is 4-bytes 
(this kind of encoding is backwards compatible with most of existing compiled methods)

