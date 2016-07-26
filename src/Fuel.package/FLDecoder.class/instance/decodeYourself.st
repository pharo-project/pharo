decoding
decodeYourself

	| objectCount |
	objectCount := self nextEncodedPositiveInteger.
	
	indexStream := FLIndexStream on: stream digits: objectCount digitLength.
	objects := Array new: objectCount.
	objectsWriteStream := WriteStream on: objects.

	isBigEndian := self nextEncodedPositiveInteger.
