*Morphic-Base
asTextMorph
	"Open a TextMorph, as best one can, on the receiver"

	^ TextMorph new contentsAsIs: self asStringOrText
