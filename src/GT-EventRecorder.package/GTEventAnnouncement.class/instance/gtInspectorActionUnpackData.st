gt-inspector-extension
gtInspectorActionUnpackData
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ :composite |
			composite selection: self unpackedData ];
		icon: GLMUIThemeExtraIcons glamorousAdd;
		condition: [ self data isNotNil ];
		title: 'Unpack data'