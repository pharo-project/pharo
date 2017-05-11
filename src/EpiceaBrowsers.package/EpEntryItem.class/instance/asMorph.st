converting
asMorph

	| morphs |
	morphs := OrderedCollection new.
	morphs addAll: self triggerMarkMorphs.
	morphs add: self icon asMorph.
	morphs addAll: self eventMorphs.
	morphs add: self morphFactory smallGap.
	morphs addAll: self timeMorphs.
	morphs addAll: self commentMorphs.

	^ self morphFactory rowWithAll: morphs