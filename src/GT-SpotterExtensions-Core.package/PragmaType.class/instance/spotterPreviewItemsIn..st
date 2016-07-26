ui
spotterPreviewItemsIn: aComposite
	<spotterPreview: 10>
	
	aComposite list
		title: '<', self gtDisplayString, '>';
		display: [ :x | x ];
		format: [ :pragma | GTSpotterItemBrick new text: pragma method gtDisplayString; disable; yourself ];
		entity: self pragmas asArray.