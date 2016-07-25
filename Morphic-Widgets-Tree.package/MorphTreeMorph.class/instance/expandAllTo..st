expanding-collapsing
expandAllTo: aLevel
	self roots do: [:m | self expand: m to: aLevel].
	self innerWidgetChanged