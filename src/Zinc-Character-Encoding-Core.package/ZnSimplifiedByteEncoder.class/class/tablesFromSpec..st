private
tablesFromSpec: mapping
	"Initialize the mappings to and from Unicode based on the 128 element array mapping"

	| byteToUnicode unicodeToByte |
	byteToUnicode := Array new: 128.
	unicodeToByte := Dictionary new.	
	"Mind the offset because first 128 characters are not stored into byteToUnicodeSpec"	
	"Note that some entries are nil"
	mapping
		keysAndValuesDo: [ :index :unicode | 
			unicode ifNotNil: [ 
				byteToUnicode at: index put: (Character value: unicode).
				unicodeToByte at: unicode put: 127 + index ] ].
	^ Array with: byteToUnicode with: unicodeToByte