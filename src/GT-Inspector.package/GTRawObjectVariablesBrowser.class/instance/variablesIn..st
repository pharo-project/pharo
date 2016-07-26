private building
variablesIn: composite
	^ composite table
		showOnly: 50;
		shouldValidate: false;
		allowsRecursiveSearch: false;
		children: [ :assoc | 
			assoc key = 'self'
				ifTrue: [ #() ]
				ifFalse: [ self variableValuePairsFor: assoc value ] ];
		icon: [ :assoc | self iconFor: assoc value ];
		display: [ :anObject | 
			self rebuildCacheFor: anObject.
			self variableValuePairsFor: anObject ];
		column: 'Variable' 
			evaluated: [:assoc | '  ', (GTObjectPrinter new asNonTruncatedTextFrom: assoc key) ];
		column: 'Value' 
			evaluated: [:assoc | GTObjectPrinter new asTruncatedTextFrom: assoc value ]
			modified: [:newValue :assoc :presentation | 
				self updateInstanceVariable: assoc key from: presentation entity basedOn: newValue.
				presentation update ];
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
			entitled: 'Open pointers to';
		dynamicActionsOnSelection: [ :presentation |
			(presentation entity class hasInstVarNamed: presentation rawSelection key)
				ifTrue: [ 
					{
						GLMGenericAction new 
								action: [:list | self systemNavigation browseAllAccessesTo: list rawSelection key from: list entity class ]; 
								title: 'List methods using this variable'.
						GLMGenericAction new 
								action: [:list | self systemNavigation browseAllStoresInto: list rawSelection key from: list entity class ]; 
								title: 'List methods storing into this variable'.		
					}
					]
				ifFalse: [ #() ] ]