submorphs-accessing
morphsAt: aPoint unlocked: aBool do: aBlock
	"Evaluate aBlock with all the morphs starting at the receiver which appear at aPoint. If aBool is true take only visible, unlocked morphs into account."
	
	(self fullBounds containsPoint: aPoint) ifFalse:[^self].
	(aBool and:[self isLocked or:[self visible not]]) ifTrue:[^self].
	self submorphsDo:[:m| | tfm |
		tfm := m transformedFrom: self.
		m morphsAt: (tfm globalPointToLocal: aPoint) unlocked: aBool do: aBlock].
	(self containsPoint: aPoint) ifTrue:[aBlock value: self].