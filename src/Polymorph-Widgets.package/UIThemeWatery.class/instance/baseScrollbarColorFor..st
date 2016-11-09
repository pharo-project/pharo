fill-styles-scrollbars
baseScrollbarColorFor: aScrollbar
	"Return the scrollbar last pane colour or that of our settings if unavailable"
	
	^ aScrollbar lastPaneColor 
		ifNil: [ self scrollbarColor ]