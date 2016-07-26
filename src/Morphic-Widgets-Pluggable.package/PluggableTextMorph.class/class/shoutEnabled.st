accessing
shoutEnabled
	^ (Smalltalk globals includesKey: #SHPreferences)
		and: [(Smalltalk globals at: #SHPreferences) enabled]