layout
localSubmorphBounds
	"Answer, in my coordinate system, the bounds of all my visible submorphs (or nil if no visible submorphs)"
	localBounds ifNil: [ 
		self hasSubmorphs ifFalse: [^ nil].
		localBounds :=  self firstSubmorph fullBounds topLeft corner: owner maxNodeWidth @ (self lastSubmorph fullBounds bottom + owner extraScrollRange)].
	^ localBounds 
