*GT-InspectorExtensions-Core
gtInspectorSourceIn: composite
	<gtInspectorPresentationOrder: 00>
	^ composite pharoMethod 
		title: 'Source';
		smalltalkClass: [ self methodClass ];
		display: [ self sourceCode ];
		act: [ :text | self methodClass compile: text text notifying: nil ]
			icon: GLMUIThemeExtraIcons glamorousAccept
			on: $s
			entitled: 'Accept';
		act: [ self browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			entitled: 'Browse'