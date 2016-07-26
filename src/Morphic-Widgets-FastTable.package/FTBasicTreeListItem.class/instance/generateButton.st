button
generateButton
	^expanded 
		ifTrue: [ self expandedButton ]
		ifFalse: [ self collapsedButton ]