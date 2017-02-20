private building
variablesIn: composite
	^ composite fastTreeTable
		hasChildren: [ :aNode | aNode key ~= 'self' ];
		children: [ :aNode | aNode childrenNodesFor: self ];
		icon: [ :aNode | self iconFor: aNode value ];
		display: [ :anObject | 
			"self rebuildCacheFor: anObject."
			self variableNodesFor: anObject ];
		column: 'Variable' translated
			evaluated: [ :aNode | (GTObjectPrinter asNonTruncatedTextFrom: aNode key) ];
		column: 'Value' translated
			evaluated: [ :aNode | GTObjectPrinter asTruncatedTextFrom: aNode value ];
		morphicSelectionAct: [:list | list selection value browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			on: $b 
			entitled: 'Browse' translated;
		morphicSelectionAct: [:list | list selection value inspect ] 
			icon: GLMUIThemeExtraIcons glamorousInspect 
			on: $i 
			entitled: 'Inspect' translated;
		dynamicActionsOnSelection: [ :presentation |
			presentation selection inspectorActionsForNode ];
		selectionAct: [:list | 
			((list selection value pointersToExcept: { list selection value })
									reject: [ :each | each pointsOnlyWeaklyTo: list selection value ]) inspect ] 
			on: $t
			entitled: 'Open pointers to' translated
	