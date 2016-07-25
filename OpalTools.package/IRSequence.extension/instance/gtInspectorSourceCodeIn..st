*OpalTools
gtInspectorSourceCodeIn: composite
	<gtInspectorPresentationOrder: 30> 

	^ composite pharoMethod 
		title: 'Source code';
		display: [ self method sourceNode source];
		smalltalkClass: [  self method sourceNode methodClass ]; 
		initialize: [ :presentation | 
			presentation selectionInterval: self sourceInterval ];
		act: [ self method compiledMethod browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'