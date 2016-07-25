accessing
showWidget
	"A FastTable can have a special function. Most of these functions are hidden by default. This method make them explicit. 
	For exemple a FastTable can have a search field. By default the user need to type to show it. With this method the widget should be alwayse visible.
	My subclasses should describe what to do to be explicit."

	self subclassResponsibility