*Polymorph-Widgets
addGripsIfWanted
	"Add the edge and corner grips if the window wants them."
	
	self wantsGrips ifTrue: [self addGrips]