updating
updateReceiverInspectorFromContext: aContext
	| receiver oldSelection newSelectionIndex   |

	oldSelection := receiverInspector inspector ifNotNil: [ :inspector | inspector list selectedItem ].

	receiver := aContext ifNotNil: [ aContext home receiver ]. 
	receiverInspector inspect: receiver.	
		
	newSelectionIndex := 1.
	oldSelection ifNotNil: 
	[	receiverInspector inspector list listItems withIndexDo: 
			[ :item :index | item label = oldSelection label ifTrue: [ newSelectionIndex := index ] ].
	].
	receiverInspector inspector list setSelectedIndex: newSelectionIndex.
	
