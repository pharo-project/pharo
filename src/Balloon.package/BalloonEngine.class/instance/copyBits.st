copying
copyBits
	(bitBlt notNil and:[bitBlt destForm notNil]) ifTrue:[bitBlt destForm unhibernate].
	self copyLoopFaster.