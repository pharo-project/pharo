*Monticello-OldDataStreamCompatibility
readWordLike
	| refPosn aSymbol newClass anObject |
	"Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc)."
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk globals at: aSymbol asSymbol.
	anObject := newClass newFromStream: byteStream.	"Size is number of long words."
	self setCurrentReference: refPosn.	"before returning to next"
	^ anObject