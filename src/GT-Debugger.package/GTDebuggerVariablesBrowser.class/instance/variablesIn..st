private building
variablesIn: composite
	^ composite fastTable
		showOnly: 50;
		shouldValidate: false;
		allowsRecursiveSearch: false;
		display: [ :context | self variablesFromContext: context ];
		column: 'Type'
			evaluated: [ '' ]
			width: 75
			tags: [ :assoc :context |
				self tagsForVariable: assoc key from: context ]
			sortedBy: [ :x :y | 
				self 
					compareVariableTypeOf: x key
					withVariable: y key
					in: ((self presentations first paneNamed: #variables) port: #entity) value];
		column: 'Variable' 
			evaluated: [:assoc | 
			[ Text fromString: assoc key asString ]
					on: Error
					do: [ Text string: 'error printing' attribute: TextColor red ] ]
			width: 175;
		column: 'Value' evaluated: [:assoc | 
			[ Text fromString: (assoc value printString contractTo: 50) ]
					on: Error
					do: [ Text string: 'error printing' attribute: TextColor red ] ];
		"send: #value;"
		morphicSelectionAct: [:list | list selection value browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			on: $b 
			entitled: 'Browse';
		morphicSelectionAct: [:list | list selection value inspect ] 
			icon: GLMUIThemeExtraIcons glamorousInspect 
			on: $i 
			entitled: 'Inspect';
		onChangeOfPort: #rawSelection 
			act: [ :aPresentation | 
				self debugger selectedVariableName: aPresentation rawSelection key ]