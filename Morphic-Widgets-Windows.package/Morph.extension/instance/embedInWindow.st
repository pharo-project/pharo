*Morphic-Widgets-Windows
embedInWindow

	| window worldToUse |

	worldToUse := self world.		"I'm assuming we are already in a world"
	window := (SystemWindow labelled: self defaultLabel) model: nil.
	window bounds: ((self position - ((0@window labelHeight) + window borderWidth))
						corner: self bottomRight + window borderWidth).
	window addMorph: self frame: (0@0 extent: 1@1).
	window updatePaneColors.
	worldToUse addMorph: window.
	window activate