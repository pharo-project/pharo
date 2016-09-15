converting
asMorph

	| morphs |
	morphs := OrderedCollection new
		add: (EpMorphFactory default emphasizedStringFor: self globalName);
		add: EpMorphFactory default smallGap;
		add: (EpMorphFactory default secondaryStringFor: self modificationTime asDate epiceaBrowsersAsString);
		yourself.

	self flag: #todo. "move to monitor"
	self globalName = EpMonitor current sessionStore store globalName ifTrue: [
		morphs add: self monitorPillMorph ].

	^ EpMorphFactory default rowWithAll: morphs