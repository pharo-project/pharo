I am ZnStringEntity, a concrete HTTP Entity based on a String.
It is used to hold textual (non-binary) data.
I am a ZnEntity.

Optionally, an encoding is used to convert to and from bytes.
The default encoding it UTF-8.

Note that content length is the encoded byte count, not the number of characters in the string.

Part of Zinc HTTP Components.