converting
asMorph

	| morphFactory |
	morphFactory := EpMorphFactory default.
	
	^ morphFactory rowWithAll: {
		morphFactory emphasizedStringFor: self globalName.
		morphFactory smallGap.
		morphFactory secondaryStringFor: self modificationTime asDate epiceaBrowsersAsString.
	}