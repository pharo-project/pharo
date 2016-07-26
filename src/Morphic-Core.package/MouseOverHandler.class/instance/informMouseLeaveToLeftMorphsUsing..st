event handling - private
informMouseLeaveToLeftMorphsUsing: anEvent 

	| asMouseLeaveEvent |
	
	asMouseLeaveEvent := anEvent asMouseLeave.
	
	leftMorphs do: [ :aLeftMorph | 
		self inform: asMouseLeaveEvent to: aLeftMorph originatedFrom: anEvent ifNotFocusedDo: [ overMorphs nextPut: aLeftMorph ] ]