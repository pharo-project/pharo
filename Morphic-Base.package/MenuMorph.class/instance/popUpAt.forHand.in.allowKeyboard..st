control
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given  
	hand."
	| evt |
	aWorld submorphs
		select: [ :each | (each isKindOf: MenuMorph)
			and: [each stayUp not]]
		thenCollect: [ :menu | menu delete].

	self items isEmpty
		ifTrue: [^ self].

	self layoutItems.
	"precompute width"
	self
		positionAt: aPoint
		relativeTo: (selectedItem
				ifNil: [self items first])
		inWorld: aWorld.
	aWorld addMorphFront: self.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean
		ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard
			or: [evt isMouse
					and: [evt anyButtonPressed not]])
		ifTrue: ["Select first item if button not down"
			self moveSelectionDown: 1 event: evt].
	self updateColor.
	self changed