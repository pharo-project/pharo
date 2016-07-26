updating
buildTopHeader
	| subs |
	subs := OrderedCollection new.
	self columns
		do: [:col |
			col container: self.
			subs add: (col header hResizing: #rigid; layoutChanged; yourself)].
	topHeader := Morph new fillStyle: self topHeaderBackground.
	self addMorph: topHeader.
	topHeader clipSubmorphs: true.
	topHeader borderColor: Color veryLightGray.
	topHeader color: Color transparent.
	topHeader borderWidth: 0.
	topHeader addAllMorphs: subs.
	topHeader bounds: (scroller left @ self top corner: scroller right @ (self top + self topHeaderHeight)).

