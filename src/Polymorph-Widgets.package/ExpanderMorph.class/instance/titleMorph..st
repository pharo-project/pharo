accessing
titleMorph: aMorph
	"Set the value of titleMorph"

	titleMorph ifNotNil: [titleMorph delete; removeDependent: self].
	titleMorph := aMorph.
	aMorph ifNotNil: [
		aMorph addDependent: self.
		 self addMorph: aMorph]