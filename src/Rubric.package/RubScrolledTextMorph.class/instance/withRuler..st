rulers handling
withRuler: aRuler
	(self hasRulerNamed: aRuler key)
		ifTrue: [ ^ self ].
	rulers add: aRuler.
	aRuler level >= 0 ifTrue: [self addMorphBack: aRuler].
	"Extra areas must be in the front "
	self extraAreas do: [ :r | self addMorphFront: r ].
	self manageLayout.
	" Finnally, the scrollPane must be in the front of all because text must be drawn last "
	self addMorphFront: self scrollPane.
	aRuler level < 0 ifTrue: [ self addMorphFront: aRuler ]