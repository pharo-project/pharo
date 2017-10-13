private
spaceUsed
	"Answer a rough estimate of number of bytes used by this class and its metaclass. Does not include space used by class variables."

	| space |
	space := 0.
	self methodsDo: [:method  |
		space := space + 16.  "dict and org'n space"
		space := space + (method size + 6 "hdr + avg pad").
		method literalsDo: [:lit |
			(lit isMemberOf: Array) ifTrue: [space := space + ((lit size + 1) * 4)].
			(lit isMemberOf: Float) ifTrue: [space := space + 12].
			(lit isMemberOf: ByteString) ifTrue: [space := space + (lit size + 6)].
			(lit isMemberOf: LargeNegativeInteger) ifTrue: [space := space + ((lit size + 1) * 4)].
			(lit isMemberOf: LargePositiveInteger) ifTrue: [space := space + ((lit size + 1) * 4)]]].
		^ space