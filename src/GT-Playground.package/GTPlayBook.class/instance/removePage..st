actions
removePage: aPage
	self pages remove: aPage ifAbsent: [ " ignore if aPage is not registered " ]