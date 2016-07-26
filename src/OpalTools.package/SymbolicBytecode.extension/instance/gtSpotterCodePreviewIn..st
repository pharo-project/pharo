*OpalTools
gtSpotterCodePreviewIn: aComposite
	<spotterPreview: 1>
	^ (aComposite pharoMethod
			title: [ self gtDisplayString ]; 
			display: [ :entity | self method sourceCode ];
			smalltalkClass: [ self method methodClass ];
			selectionInterval: [ self sourceInterval ];
			entity: self) 