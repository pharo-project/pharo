*OpalTools
gtInspectorSourceCodeIn: composite
	<gtInspectorPresentationOrder: 30> 

	^ composite pharoMethod 
		title: 'Source';
		display: [ self method sourceCode ];
		smalltalkClass: [ self method methodClass ]; 
		initialize: [ :presentation | 
			presentation selectionInterval: self sourceInterval ];
		act: [ self method compiledMethod browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'