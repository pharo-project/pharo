accessing
do: aBlock
	| nn |
	nn := self.
	
	[ nn isNil ] whileFalse: [ 
		aBlock value: nn.
		nn := nn next. ]