updating
update: aParameter 
	"Refer to the comment in View|update:."

	(aParameter == getListSelector or: [ aParameter == getListElementSelector ]) ifTrue: 
		[self updateList.
		^ self].
	aParameter == getIndexSelector ifTrue:
		[self selectionIndex: self getCurrentSelectionIndex.
		^ self].
	aParameter == #allSelections ifTrue:
		[self selectionIndex: self getCurrentSelectionIndex.
		^ self changed].
	
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #setMultipleSelection: ifTrue: [ self setMultipleSelection: aParameter second ]