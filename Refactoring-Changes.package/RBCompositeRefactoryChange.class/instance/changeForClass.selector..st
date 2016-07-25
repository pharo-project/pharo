accessing
changeForClass: aClassName selector: aSelector 
	changes reverseDo: [ :each | 
		| change |
		change := each changeForClass: aClassName selector: aSelector.
		change notNil ifTrue: [ ^ change ] ].
	^ nil