private building
variablesIn: composite
	^ composite fastTable
		useCache;
		display: [ :aContext | self variablesFromContext: aContext. ];
		column: 'Type'
			evaluated: [ '' ]
			width: 75
			tags: [ :aNode  | { aNode variableTag } ]
			sortedBy: [ :x :y |  x variableTag < y  variableTag ];
		column: 'Variable' 
			evaluated: [ :aNode | GTObjectPrinter asNonTruncatedTextFrom: aNode key ]
			width: 175;
		column: 'Value' 
			evaluated: [ :aNode | | rawValue displayValue |
				[ rawValue := aNode rawValue. ] 
					on: Error 
					do: [ displayValue := Text string: aNode errorMessage attribute: TextColor red ].
				displayValue ifNil: [ GTObjectPrinter asTruncatedTextFrom: rawValue ] ];
		dynamicActionsOnSelection: [ :presentation |
			(presentation rawSelection isKindOf: GTInspectorVariableNode) 
				ifTrue: [presentation rawSelection inspectorActionsForNode ]
				ifFalse: [ #() ] ];
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