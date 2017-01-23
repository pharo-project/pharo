others
morphWithCustomInteraction
	"self new morphWithCustomInteraction openOn: 42"

	<glmBrowser: 'Morph with custom interaction' input: '42'>
	^ GLMCompositePresentation new tabulator
		with: [ :tabulator | 
			tabulator
				column: #morph;
				column: #preview.
			tabulator transmit
				to: #morph;
				andShow: [ :composite | 
							composite morph
								morph: [ :morphPresentation | 
									| button |
									(button := SimpleButtonMorph new)
										on: #mouseUp send: #value to: [ morphPresentation selection: 'You just clicked' ];
										label: 'I am a button. Please click me';
										yourself ] ].
			tabulator transmit
				from: #morph;
				to: #preview;
				andShow: [ :a | a text ] ]