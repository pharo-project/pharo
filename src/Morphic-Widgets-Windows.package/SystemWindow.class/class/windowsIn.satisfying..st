top window
windowsIn: aWorld satisfying: windowBlock
	| windows |

	windows := OrderedCollection new.
	aWorld ifNil: [^windows].	"opening MVC in Morphic - WOW!"
	aWorld submorphs do:
		[:m | | s |
		((m isSystemWindow) and: [windowBlock value: m])
			ifTrue: [windows addLast: m]
			ifFalse: [((m isKindOf: TransformationMorph) and: [m submorphs size = 1])
					ifTrue: [s := m firstSubmorph.
							((s isSystemWindow) and: [windowBlock value: s])
								ifTrue: [windows addLast: s]]]].
	^ windows