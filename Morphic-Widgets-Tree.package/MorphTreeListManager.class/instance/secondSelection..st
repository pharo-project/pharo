selection accessing
secondSelection: anItem

 	| nodeMorph |
	nodeMorph := self allNodeMorphs detect: [ :e | e complexContents item == anItem ].
	self searchedElement: nodeMorph.
	client changed.