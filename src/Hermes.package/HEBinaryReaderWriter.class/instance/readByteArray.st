reading
readByteArray
	| byteArray size |
	size := self readInt32. 
	byteArray := stream next: size.
	^ byteArray