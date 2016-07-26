enumerating
reverseDo: aBlock
	| span |
	span := self.
	
	[ aBlock value: span. span := span prevIfNone: [ ^ self ] ] repeat