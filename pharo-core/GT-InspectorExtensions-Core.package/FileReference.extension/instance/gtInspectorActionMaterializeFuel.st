*GT-InspectorExtensions-Core
gtInspectorActionMaterializeFuel
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ :composite | 
			| result |
			result := FLMaterializer materializeFromFileNamed: self fullName.
			composite selection: result ];
		icon: GLMUIThemeExtraIcons glamorousAdd;
		condition: [ self isFile and: [self extension = 'fuel' ] ];
		title: 'Materialize'