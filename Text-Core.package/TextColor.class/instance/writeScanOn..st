scanning
writeScanOn: strm
	"Two formats.  c125000255 or cblue;"

	| nn |
	strm nextPut: $c.
	(nn := color name) ifNotNil: [
		(self class respondsTo: nn) ifTrue: [
			^ strm nextPutAll: nn; nextPut: $;]].
	(Array with: color red with: color green with: color blue) do: [:float | | str |
		str := '000', (float * 255) asInteger printString.
		strm nextPutAll: (str copyFrom: str size-2 to: str size)]