meta-actions
inspectAt: aPoint event: evt
	|  morphs target |
	morphs := self morphsAt: aPoint.
	(morphs includes: self) ifFalse:[morphs := morphs copyWith: self].
	target := UIManager default
				chooseFrom: (morphs
						collect: [:t |t class name asString])
				values: morphs
				title: ('inspect whom? (deepest at top)' translated).
	target ifNil:[^self].
	target inspectInMorphic: evt