drawing
copyBits
	(bitBlt notNil and:[bitBlt destForm notNil]) ifTrue:[bitBlt destForm unhibernate].
	self copyLoopFastest.