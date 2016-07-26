accessing
style	
	^ text isEmpty
		ifTrue: [ text defaultStyle ]
		ifFalse: [ next style ]