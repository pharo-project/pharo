Extension { #name : #SimpleHierarchicalListMorph }

{ #category : #'*Polymorph-Widgets-Core' }
SimpleHierarchicalListMorph >> themeChanged [
	"Update the selection colour."

	self selectionColor: self theme selectionColor.
	self color: self defaultColor.
	super themeChanged
]
