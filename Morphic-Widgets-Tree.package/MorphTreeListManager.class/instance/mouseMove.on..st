mouse managing
mouseMove: evt on: aTargetMorph

	(aTargetMorph isNil or: [aTargetMorph highlightedForMouseDown not])
		ifTrue: [self allNodeMorphs do: [:m |
				m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
				aTargetMorph ifNotNil: [aTargetMorph highlightForMouseDown.
				(self autoMultiSelection and: [evt shiftPressed not])
					ifTrue: [
						self autoMultiSelect: aTargetMorph.
						self selectionChanged]]]