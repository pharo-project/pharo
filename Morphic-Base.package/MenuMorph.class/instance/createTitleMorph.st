construction-title
createTitleMorph

	titleMorph ifNotNil: [ titleMorph delete ].
	titleMorph := MenuTitleMorph new.
	
	titleMorph 
	on: #mouseDown send: #mouseDownInTitle: to: self.
	"maxCellSize:World width /2;"
	
	self addMorphFront: titleMorph.