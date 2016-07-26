meta-actions
copyToPasteBuffer: aMorph
	"Save this morph in the paste buffer. This is mostly useful for copying morphs between projects."
	aMorph ifNil:[^PasteBuffer := nil].
	Cursor wait showWhile:[
		PasteBuffer := aMorph topRendererOrSelf veryDeepCopy.
		PasteBuffer privateOwner: nil].

