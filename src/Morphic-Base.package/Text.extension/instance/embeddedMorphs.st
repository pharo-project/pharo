*Morphic-Base
embeddedMorphs
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs withStartStopAndValueDo: 
			[:start :stop :attribs | 
			attribs 
				do: [:attrib | attrib anchoredMorph ifNotNil: [morphs add: attrib anchoredMorph]]].
	^morphs select: [:m | m isMorph]