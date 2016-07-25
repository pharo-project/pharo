*Polymorph-Widgets
setLabelWidgetAllowance
	"Set the extra space required, in general, apart from the label.
	Should make theme based (for centered titles), leave enough room
	for the moment."
	
	^labelWidgetAllowance :=  (self boxExtent x * 7) 
	"allow for three on one side and centering plus a bit"