event handling
doubleClickEvent: anEvent onNodeMorph: aNodeMorph
	(self canBeRenamed and: [ anEvent shiftPressed ])
		ifTrue:
			[ self model model selectedPackage: aNodeMorph complexContents item.
			self openFloatingEditorToRenameFromNodeMorph: aNodeMorph ]