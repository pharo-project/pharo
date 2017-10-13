instance creation
derivedFrom: aForm      "Cursor initNormalWithMask.  Cursor normal show"
	"aForm is presumably a cursor"
	| cursor mask ext |
	ext := aForm extent.
	cursor := self extent: ext.
	cursor copy: (1@1 extent: ext) from: 0@0 in: aForm rule: Form over.
	mask := Form extent: ext.
	(1@1) eightNeighbors do:
		[:p | mask copy: (p extent: ext) from: 0@0 in: aForm rule: Form under].
	cursor setMaskForm: mask.
	cursor offset: ((aForm offset - (1@1)) max: ext negated).
	^ cursor