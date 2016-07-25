model access
itemSelectedAmongMultiple: index
	"return whether the index-th row is selected.  Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany"

	^ self isMultipleSelection
		ifTrue: [ (self listSelectionAt: index) == true ]
		ifFalse: [ false ]