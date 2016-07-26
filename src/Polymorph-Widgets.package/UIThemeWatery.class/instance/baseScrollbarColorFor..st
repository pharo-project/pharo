fill-styles-scrollbars
baseScrollbarColorFor: aScrollbar
	"Return the scrollbar last pane colour or that of our settings if unavailable"
	
	^ (aScrollbar valueOfProperty: #lastPaneColor) 
		ifNil: [ self scrollbarColor ]