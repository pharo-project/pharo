enumerating
do: aBlock
	| span |
	span := self.
	[ aBlock value: span. span := span nextIfNone: [ ^ self ] ] repeat.