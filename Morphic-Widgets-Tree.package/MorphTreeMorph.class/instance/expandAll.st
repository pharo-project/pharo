expanding-collapsing
expandAll
	"Expand all of the roots"
	
	self roots reverseDo: [:m | self expandAllSilently: m].
	self innerWidgetChanged