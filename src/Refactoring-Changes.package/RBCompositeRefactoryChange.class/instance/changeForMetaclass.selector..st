accessing
changeForMetaclass: aClassName selector: aSelector 
	changes reverseDo: [ :each | 
		| change |
		change := each changeForMetaclass: aClassName selector: aSelector.
		change notNil ifTrue: [ ^ change ] ].
	^ nil