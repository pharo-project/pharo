icon
rebuildUpAndDownArrowIconFrom: array

	| container |
	
	container := Morph new.
	container 
		extent: 16@16;
		color: Color transparent;
		changeTableLayout;
		wrapCentering: #center;
		cellPositioning: #topCenter;
		listDirection: #topToBottom.
	
	array 
	collect: [ :each | each asIconTargetting: NautilusUI ] 
	thenDo: [ :each | container addMorphBack: each ].
	
	^container