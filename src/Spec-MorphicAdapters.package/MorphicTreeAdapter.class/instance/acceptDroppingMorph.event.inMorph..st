drag and drop - private
acceptDroppingMorph: draggedMorph event: event inMorph: source
	| item |
	
	item := self widget
		ifNil:  [ 0 ]
		ifNotNil: [:w | w scrollerSubMorphFromPoint: event position ].

	^ self acceptDropBlock valueWithEnoughArguments: { draggedMorph model model . event. source model model model. item model nodeModel }