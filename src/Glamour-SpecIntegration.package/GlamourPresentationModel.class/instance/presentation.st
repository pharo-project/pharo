accessing
presentation
	presentation isNil ifTrue:[
		| morph |
		morph := GLMMorphicRenderer new render: self glmPres.
		morph hResizing: #spaceFill.
		morph vResizing: #spaceFill.
		presentation := MorphicGenericAdapter morph: morph].

	^ presentation