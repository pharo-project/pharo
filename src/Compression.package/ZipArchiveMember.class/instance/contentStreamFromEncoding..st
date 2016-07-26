accessing
contentStreamFromEncoding: encodingName
	"Answer my contents as a text stream.
	Interpret the raw bytes with given encodingName"

	| s |
	s := MultiByteBinaryOrTextStream on: (String new: self uncompressedSize).
	s converter:  (TextConverter newForEncoding: encodingName).
	self extractTo: s.
	s reset.
	^ s
