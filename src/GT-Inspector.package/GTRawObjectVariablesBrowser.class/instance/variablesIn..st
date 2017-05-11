private building
variablesIn: composite
	^ composite fastTreeTable
		useCache;
		hasChildren: [ :aNode | aNode key ~= 'self' ];
		children: [ :aNode | aNode childrenNodesFor: self ];
		icon: [ :aNode | self iconFor: aNode value ];
		display: [ :anObject | 
			self variableNodesFor: anObject ];
		column: 'Variable' translated
			evaluated: [ :aNode | (GTObjectPrinter asNonTruncatedTextFrom: aNode label) ];
		column: 'Value' translated
			evaluated: [ :aNode | | rawValue displayValue |
				[ rawValue := aNode rawValue. ] 
					on: Error 
					do: [ displayValue := Text string: aNode errorMessage attribute: TextColor red ].
				displayValue ifNil: [ GTObjectPrinter asTruncatedTextFrom: rawValue ] ]
			modified: [ :newValue :editedNode :curentPresentation | 
				self updateVariableNode: editedNode basedOn: newValue.
				curentPresentation requestRefresh ];
		morphicSelectionAct: [:list | list selection value browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			on: $b 
			entitled: 'Browse' translated;
		morphicSelectionAct: [:list | list selection value inspect ] 
			icon: GLMUIThemeExtraIcons glamorousInspect 
			on: $i 
			entitled: 'Inspect' translated;
		dynamicActionsOnSelection: [ :presentation |
			(presentation rawSelection isKindOf: GTInspectorVariableNode) 
				ifTrue: [presentation rawSelection inspectorActionsForNode ]
				ifFalse: [ #() ] ];
		selectionAct: [:list | 
			((list selection value pointersToExcept: { list selection value })
									reject: [ :each | each pointsOnlyWeaklyTo: list selection value ]) inspect ] 
			on: $t
			entitled: 'Open pointers to' translated
	