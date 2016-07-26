*OpalTools
gtInspectorSourceCodeIn: composite
	<gtInspectorPresentationOrder: 30> 

	^ composite pharoMethod 
		title: 'Source';
		display: [ self node methodNode sourceCode ];
		smalltalkClass: [ self node methodNode  methodClass ]; 
		initialize: [ :presentation | 
			presentation selectionInterval: self sourceInterval ];
		act: [ self method compiledMethod browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'