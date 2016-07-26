class initialization
initializeTables
	"Initialize the mappings to and from Unicode."

	byteToUnicode := Array new: 128.
	unicodeToByte := Dictionary new.
	"Mind the offset because first 128 characters are not stored into byteToUnicodeSpec"
	"Some entries are nil, make them pass-through for compatibility, for now"
	self byteToUnicodeSpec keysAndValuesDo: [ :index :unicode | | unicodeValue |
		unicodeValue := unicode ifNil: [ 127 + index ] ifNotNil: [ unicode ].
		byteToUnicode at: index put: (Character value: unicodeValue).
		unicodeToByte at: unicodeValue put: (127 + index) asCharacter ]