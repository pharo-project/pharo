private
back16BitWordOnStream: stream
	| firstByte secondByte |
	firstByte := stream back.
	secondByte := stream back.
	^ self isBigEndian 
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]