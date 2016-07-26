*Morphic-Widgets-FastTable
scrollRestrictedUp: aValue

	"only scroll if the scrollbar is not at its end yet"

	self canScrollUp ifFalse: [ ^self ].
	
	self scrollUp: aValue