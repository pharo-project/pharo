enumerating
last
	| span |
	span := self.
	[ span next == span ] whileFalse: [ span := span next ].
	^ span