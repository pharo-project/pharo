accessing
nextLineSeparatorIfNone: aBlock
	"Find a nearest line separator next to receiver or evaluate a block if none found"
	
	| span |
	span := self.
	[ span := span nextIfNone: aBlock. span isLineSeparator ] whileFalse.
	^ span