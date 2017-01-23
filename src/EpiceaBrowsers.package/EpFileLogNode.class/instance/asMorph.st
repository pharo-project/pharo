converting
asMorph

	| morphs |
	morphs := OrderedCollection new
		add: (EpMorphFactory default emphasizedStringFor: self globalName);
		add: EpMorphFactory default smallGap;
		add: (EpMorphFactory default secondaryStringFor: self modificationTime asDate epiceaBrowsersAsString);
		yourself.

	^ EpMorphFactory default rowWithAll: morphs