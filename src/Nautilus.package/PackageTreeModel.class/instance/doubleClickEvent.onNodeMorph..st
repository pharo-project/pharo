event handling
doubleClickEvent: anEvent onNodeMorph: aNodeMorph
	self selectedNode ifNotNil: [ :n | n doubleClickEvent: anEvent onNodeMorph: aNodeMorph ]