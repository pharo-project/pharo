morphic
acceptDroppingMorph: aMorph event: evt
	| item dropTarget |

	dropItemSelector 
		ifNil:[
			model 
				acceptDroppingMorph: aMorph 
				event: evt 
				inMorph: self ]
		ifNotNil: [
			item := aMorph passenger.
			dropTarget := (self itemFromPoint: evt position) withoutListWrapper.
			model perform: dropItemSelector with: item with: dropTarget ].
		
	evt hand releaseMouseFocus: self.
	Cursor normal show.
