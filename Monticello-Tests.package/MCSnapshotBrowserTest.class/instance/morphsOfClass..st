morphic
morphsOfClass: aMorphClass
	| morphs |
	morphs := OrderedCollection new.
	morph allMorphsDo: [:m | (m isKindOf: aMorphClass) ifTrue: [morphs add: m]].
	^ morphs