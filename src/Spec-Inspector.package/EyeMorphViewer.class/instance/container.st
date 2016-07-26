accessing
container
	^ container ifNil: [ container := (PanelMorph new 
		changeProportionalLayout)
		asSpecAdapter
		 vSpaceFill;
		hSpaceFill]