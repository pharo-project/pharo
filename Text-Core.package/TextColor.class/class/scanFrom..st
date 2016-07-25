instance creation
scanFrom: strm
	"read a color in the funny format used by Text styles on files. c125000255 or cblue;"

	| r g b |
	strm peek isDigit
		ifTrue:
			[r := (strm next: 3) asNumber.
			g := (strm next: 3) asNumber.
			b := (strm next: 3) asNumber.
			^ self color: (Color r: r g: g b: b range: 255)].
	"A name of a color"
	^ self color: (Color perform: (strm upTo: $;) asSymbol)