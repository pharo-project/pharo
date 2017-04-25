*GT-InspectorExtensions-Core
gtInspectorMorphExtensionIn: composite
	<gtInspectorPresentationOrder: 15>
	
	^ composite table
		title: 'Extension';
		showOnly: 50; 
		display: [ self extension gtInspectorVariableValuePairs asSortedCollection ];
		when: [ extension notNil ];
		column: 'Variable' 
			evaluated: [:assoc | GTObjectPrinter asNonTruncatedTextFrom: assoc key ];
		column: 'Value' 
			evaluated: [:assoc | GTObjectPrinter asTruncatedTextFrom: assoc value ];
		send: #value;
		morphicSelectionAct: [:list | list selection value browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			on: $b 
			entitled: 'Browse';
		morphicSelectionAct: [:list | list selection value inspect ] 
			icon: GLMUIThemeExtraIcons glamorousInspect 
			on: $i 
			entitled: 'Inspect';
		selectionAct: [:list | 
			((list selection pointersToExcept: { list selection })
									reject: [ :each | each pointsOnlyWeaklyTo: list selection ]) inspect ] 
			on: $t
			entitled: 'Open pointers to'