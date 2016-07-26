drag and drop
acceptDroppingMorph: aMorph event: evt inMorph: aTreeMorph
	| target items |
	
	target := (aTreeMorph scrollerSubMorphFromPoint: evt position) complexContents item.
	items := aMorph passenger.
	target ifNotNil: [ 
		model 
			dropInAPackage: items 
			into: target ]