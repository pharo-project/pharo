api-events
whenEnabledChanged: aBlock
	"Set a block to performed when the widget is enabled or disabled"
	
	enabledHolder whenChangedDo: aBlock