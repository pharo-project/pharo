instance creation
render: aPresentation from: aRenderer
	^ self new 
		renderer: aRenderer;
		render: aPresentation