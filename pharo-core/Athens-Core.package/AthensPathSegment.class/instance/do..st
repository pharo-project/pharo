iterating
do: aBlock
	| segment |
	
	segment := self.
	
	[  aBlock value: segment. segment := segment next. segment notNil ] whileTrue.