*Morphic-Widgets-FastTable
scrollRestrictedDown: aValue

	"only scroll if the scrollbar is not at its end yet"

	self canScrollDown ifFalse: [ ^self ].
	
	self scrollDown: aValue