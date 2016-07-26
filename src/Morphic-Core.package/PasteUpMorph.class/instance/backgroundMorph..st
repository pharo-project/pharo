accessing
backgroundMorph: aMorph
	"Set the background morph.
	Probably best if locked prior to adding."

	self backgroundMorph isNil ifFalse: [
		self backgroundMorph delete].
	backgroundMorph := aMorph.
	aMorph isNil ifFalse: [
		aMorph bounds: self bounds.
		self addMorphBack: aMorph]