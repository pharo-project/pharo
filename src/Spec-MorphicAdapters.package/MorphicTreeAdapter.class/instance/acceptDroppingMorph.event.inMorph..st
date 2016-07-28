drag and drop - private
acceptDroppingMorph: draggedMorph event: event inMorph: source
	| item |
	item := self widget ifNotNil: [ :w | w scrollerSubMorphFromPoint: event position ].
	item ifNil: [ ^ self ].
	^ self acceptDropBlock
		valueWithEnoughArguments: { draggedMorph model model. event. source model model model. item model nodeModel}