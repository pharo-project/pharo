rulers handling
withoutRuler: aRuler
	(self hasRuler: aRuler)
		ifFalse: [ ^ self ].
	rulers remove: aRuler.
	self removeMorph: aRuler.
	self manageLayout.
	self changed