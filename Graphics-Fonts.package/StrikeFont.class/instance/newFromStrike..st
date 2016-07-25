file in/out
newFromStrike: fileName 
	"Build an instance from the strike font file name. The '.strike' extension
	is optional."
	| strike startName raster16  |
	name := fileName copyUpTo: $..	"assumes extension (if any) is '.strike'"
	strike := FileStream readOnlyFileNamed: name , '.strike.'.
	strike binary.

	"strip off direcory name if any"
	startName := name size.
	
	[ startName > 0 and: [ (name at: startName) ~= $> & ((name at: startName) ~= $]) ] ] whileTrue: [ startName := startName - 1 ].
	name := name 
		copyFrom: startName + 1
		to: name size.
	type := strike nextWord.	"type is ignored now -- simplest
												assumed.  Kept here to make
												writing and consistency more
												straightforward."
	minAscii := strike nextWord.
	maxAscii := strike nextWord.
	maxWidth := strike nextWord.
	strike nextWord. "strikeLength"
	ascent := strike nextWord.
	descent := strike nextWord.
	strike nextWord. "xOffset"
	raster16 := strike nextWord.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	glyphs := Form 
		extent: (raster16 * 16) @ self height
		offset: 0 @ 0.
	glyphs bits fromByteStream: strike.
	xTable := (Array new: maxAscii + 3) atAllPut: 0.
	(minAscii + 1 to: maxAscii + 3) do: 
		[ :index | 
		xTable 
			at: index
			put: strike nextWord ].

	"Set up space character"
	((xTable at: Space asciiValue + 2) = 0 or: [ (xTable at: Space asciiValue + 2) = (xTable at: Space asciiValue + 1) ]) ifTrue: 
		[ Space asciiValue + 2 
			to: xTable size
			do: 
				[ :index | 
				xTable 
					at: index
					put: (xTable at: index) + DefaultSpace ] ].
	strike close.
	characterToGlyphMap := nil