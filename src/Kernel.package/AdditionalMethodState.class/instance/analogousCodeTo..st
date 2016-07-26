testing
analogousCodeTo: aMethodProperties
	| bs |
	self class == aMethodProperties class ifFalse:
		[^false].
	(bs := self basicSize) = aMethodProperties basicSize ifFalse:
		[^false].
	1 to: bs do:
		[:i|
		((self basicAt: i) analogousCodeTo: (aMethodProperties basicAt: i)) ifFalse:
			[^false]].
	^true