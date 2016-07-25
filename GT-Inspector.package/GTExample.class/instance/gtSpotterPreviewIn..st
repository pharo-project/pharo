ui
gtSpotterPreviewIn: aComposite
	<spotterPreview: 1>
	^ aComposite pharoMethod
		title: [ self gtDisplayString ]; 
		display: [ :entity | entity method sourceCode ];
		smalltalkClass: [ self method methodClass ];
		entity: self