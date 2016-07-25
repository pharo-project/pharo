layout
layoutBounds
	"Bounds of pane area only."
	| box |

	box := super layoutBounds.
	^box withTop: box top + self labelHeight