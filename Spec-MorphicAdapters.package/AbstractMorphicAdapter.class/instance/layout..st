protocol
layout: aLayout
	| layout |
	
	layout := aLayout asMorphicLayout.

	self widgetDo: [ :w | w layoutFrame: layout ]