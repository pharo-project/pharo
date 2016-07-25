enumerating
first
	| span |
	span := self.
	[ span isAtStart ] whileFalse: [ span := span prev ].
	^ span