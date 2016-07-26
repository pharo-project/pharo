*GT-SpotterExtensions-Core
gtSpotterCodePreviewIn: aComposite
	<spotterPreview: 10>
	self source ifNil: [ ^ self ].
		
	^ aComposite pharoMethod
			title: 'Source code';
			display: [ self source ];
			smalltalkClass: [ self parents first compilationContext ifNotNil: [ :ctx | ctx getClass ] ];
			selectionInterval: [ self sourceInterval ];
			entity: self