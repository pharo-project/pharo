testing
announcesNewSelection 
	"ContextChanged announces a new selection if 
	the updated port is #selection and the new value is different from the current selection. 
	This method is useful when updating the selection in UI widgets, because 
	we only want to affect the widget when there actually is a new selection" 
	 
	^self property = #selection 
		and: [self presentation selection ~~ self value]