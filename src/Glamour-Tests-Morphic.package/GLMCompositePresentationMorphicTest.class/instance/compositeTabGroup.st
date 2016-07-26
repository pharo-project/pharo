private
compositeTabGroup
	|composite|
	composite := self compositeMorph.
	self assert: (composite isKindOf: LazyTabGroupMorph).
	^ composite
