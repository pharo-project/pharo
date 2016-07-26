*Morphic-Base
embeddedMorphsFrom: start to: stop 
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs 
		runsFrom: start
		to: stop
		do: 
			[:attribs | 
			attribs 
				do: [:attr | attr anchoredMorph ifNotNil: [morphs add: attr anchoredMorph]]].
	^morphs select: [:m | m isMorph]