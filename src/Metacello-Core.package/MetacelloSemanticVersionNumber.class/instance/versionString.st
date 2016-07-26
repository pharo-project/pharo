accessing
versionString

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents