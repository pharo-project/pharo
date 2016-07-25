*Polymorph-Widgets
hideOrShowScrollBar
	"Hide or show the scrollbar depending on if the pane is scrolled/scrollable."

	"Don't show it if we were told not to."
	(self valueOfProperty: #noScrollBarPlease ifAbsent: [false]) ifTrue: [^self].

	(self vIsScrollbarNeeded not and: [ self isScrolledFromTop not ]) ifTrue: [self vHideScrollBar].
	(self vIsScrollbarNeeded or: [ self isScrolledFromTop ]) ifTrue: [self vShowScrollBar].
