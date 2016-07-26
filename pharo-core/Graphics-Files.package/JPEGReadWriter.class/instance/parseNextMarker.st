marker parsing
parseNextMarker
	"Parse the next marker of the stream"
	| byte discardedBytes |
	discardedBytes := 0.
	[ (byte := self next) = 255 ] whileFalse: [ discardedBytes := discardedBytes + 1 ].
	
	[ [ (byte := self next) = 255 ] whileTrue.
	byte = 0 ] whileTrue: [ discardedBytes := discardedBytes + 2 ].
	discardedBytes > 0 ifTrue: 
		[ "notifyWithLabel: 'warning: extraneous data discarded'"
		self ].
	self perform: (JFIFMarkerParser 
			at: byte
			ifAbsent: 
				[ (self okToIgnoreMarker: byte) 
					ifTrue: [ #skipMarker ]
					ifFalse: [ self error: 'marker ' , byte printStringHex , ' cannot be handled' ] ])