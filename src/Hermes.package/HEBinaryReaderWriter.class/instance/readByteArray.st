reading
readByteArray
	| byteArray size |
	size := self readInt. 
	byteArray := stream next: size.
	^ byteArray