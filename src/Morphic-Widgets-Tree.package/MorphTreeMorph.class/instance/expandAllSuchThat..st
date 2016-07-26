expanding-collapsing
expandAllSuchThat: aBlock
	self roots do: [:m | self expand: m suchThat: aBlock].
	self innerWidgetChanged