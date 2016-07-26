construction
addLine
	"Append a divider line to this menu. Suppress duplicate lines."
	(self hasItems and: [ self lastSubmorph isMenuLineMorph not ])
		ifTrue: [ self addMorphBack: MenuLineMorph new ].
		
		
		
	"(self hasItems)
		ifTrue: [ self addMorphBack: MenuLineMorph new .self addMorphBack: MenuLineMorph new]."
		
	"Looks ok when allowing more than one line after another line...."