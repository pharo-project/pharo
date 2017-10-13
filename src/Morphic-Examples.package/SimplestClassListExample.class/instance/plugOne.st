examples
plugOne
	| l |
	rootClass := Object.
	l := PluggableListMorph
		on: self 
		list: #rootItems
		selected: #selected
		changeSelected: #selected:.
	(l embeddedInMorphicWindowLabeled: 'test') openInWorld