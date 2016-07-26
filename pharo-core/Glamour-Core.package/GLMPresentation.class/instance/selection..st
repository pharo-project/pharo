accessing ports
selection: anObject
	"Tell the pane about the new selection. This method is called when the user changes a view's selection' (e.g., he clicks on an element in a list morph)." 
	(self pane port: #rawSelection) value: anObject.
	self acceptsSelection ifFalse: [^ self].
	rawSelectionTransmissions do: #transmit