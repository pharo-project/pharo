accessing
buttonMorph: aMorph
	"Set the value of buttonMorph"

	buttonMorph ifNotNil: [
		self removeDependent: buttonMorph.
		buttonMorph delete].
	buttonMorph := aMorph.
	self
		addDependent: aMorph;
		addMorphFront: aMorph