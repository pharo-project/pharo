ui
spotterPreviewIn: aComposite
	<spotterPreview: 10>
	aComposite list
		title: 'Preview';
		display: [ :x | x ];
		format: [ :method | GTSpotterItemBrick new text: method gtDisplayString; disable; yourself ];
		entity: self implementors