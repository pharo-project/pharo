grabbing/dropping
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	delta := m bounds extent // 2.
	m position: (self position - delta).
	m formerPosition: m position.
	targetOffset := m position - self position.
	self addMorphBack: m.