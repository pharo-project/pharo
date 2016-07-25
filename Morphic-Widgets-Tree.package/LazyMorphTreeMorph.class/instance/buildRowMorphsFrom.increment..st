initialize - release
buildRowMorphsFrom: aNodeMorph increment: anIncrement
	
	Cursor wait 
		showWhile: [ | subs idx max |
			subs := self allNodeMorphs.
			idx :=  aNodeMorph index.
			max := (idx + anIncrement) min: subs size.
			self buildRowMorphsFromIndex: idx to: max].