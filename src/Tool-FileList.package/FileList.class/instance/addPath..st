private
addPath: aString
	"Add the given string to the list of recently visited directories."

	| full |
	aString ifNil: [^self].
	full := String streamContents: [ :strm | 
		2 to: volumeList size do: [ :i | 
			strm nextPutAll: (volumeList at: i) trimBoth.
			strm nextPut: FileSystem disk separator]].
	full := full, aString.