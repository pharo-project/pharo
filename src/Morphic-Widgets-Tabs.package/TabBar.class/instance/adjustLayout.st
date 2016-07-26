drawing
adjustLayout
	| length overlap |
	
	length := self computeLength.
	self removeAllMorphs.
	length isZero ifTrue: [ ^ self ].
	overlap := self overlap.	
	
	selectionHistory do: [ :tab || index |
		index := (tabs indexOf: tab) - 1.
		self 
			addMorph: tab
			fullFrame: (LayoutFrame identity
				rightFraction: 0;
				leftOffset: (index*(length-overlap));
				rightOffset: (index*(length-overlap)) + length;
				bottomOffset: -1;
				yourself ).
		tab updateShowIcon ].
	
	self addActionsFor: self selectedTab.