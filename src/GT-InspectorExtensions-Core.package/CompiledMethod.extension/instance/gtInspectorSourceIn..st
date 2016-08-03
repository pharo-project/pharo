*GT-InspectorExtensions-Core
gtInspectorSourceIn: composite
	<gtInspectorPresentationOrder: 30>
	^ composite pharoMethod
		title: 'Source';
		smalltalkClass: [ self methodClass ];
		display: [ self sourceCode ];
		act: [ self browse ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse'