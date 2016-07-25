structure
morphPreceding: aSubmorph
	"Answer the morph immediately preceding aSubmorph, or nil if none"

	| anIndex |
	anIndex := submorphs indexOf: aSubmorph ifAbsent: [^ nil].
	^ anIndex > 1
		ifTrue:
			[submorphs at: (anIndex - 1)]
		ifFalse:
			[nil]