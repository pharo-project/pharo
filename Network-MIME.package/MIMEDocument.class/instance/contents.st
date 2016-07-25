accessing
contents
	"Answer the receiver's raw data. If we have a stream to read from. Read in the data, cache it and discard the stream."

	contents ifNil: [contents := self getContentFromStream].
	^contents