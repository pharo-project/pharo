expanding-collapsing
expandAll: aMorph
	|  subs |
	self expandAllSilently: aMorph.
	aMorph updateChildrenRecursively.
	subs := self scroller submorphs.
	1 to: subs size do: [:pos | (subs at: pos) index: pos].
	"set the new morphs widths according to columns width"
	self innerWidgetChanged.

