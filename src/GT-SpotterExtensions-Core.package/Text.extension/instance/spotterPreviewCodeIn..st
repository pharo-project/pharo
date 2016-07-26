*GT-SpotterExtensions-Core
spotterPreviewCodeIn: aComposite
	<spotterPreview: 10>
	^ aComposite text
		title: 'Contents';
		display: [ :item | item asString contractTo: 100 ];
		entity: self