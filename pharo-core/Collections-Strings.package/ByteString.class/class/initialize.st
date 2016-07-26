initialization
initialize
	"ByteString initialize"

	NonAsciiMap := ByteArray new: 256.
	0 to: 255 do: [ :i | 
		i < 128
			ifTrue: [ NonAsciiMap at: i + 1 put: 0	"valid ascii subset" ]
			ifFalse: [ NonAsciiMap at: i + 1 put: 1	"extended charset" ] ]