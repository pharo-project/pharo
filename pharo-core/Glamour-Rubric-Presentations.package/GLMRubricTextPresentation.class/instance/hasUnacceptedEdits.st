actions text
hasUnacceptedEdits

	self announce: (GLMTextEditorRequest new 
		action: [ :anEditor | ^ anEditor model hasUnacceptedEdits])