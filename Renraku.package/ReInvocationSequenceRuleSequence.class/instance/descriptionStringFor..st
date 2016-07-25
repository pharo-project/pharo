printing
descriptionStringFor: aSet

	^ aSet size = 1
		ifTrue: [ '#', aSet anyOne, ' message' ]
		ifFalse: [ 'any one of #', (aSet joinUsing: ', #'), ' messages' ]