geometry
panelRect
	"Answer the area below the title bar which is devoted to panes."

	^ self innerBounds insetBy: (0 @ self labelHeight corner: 0 @ 0)