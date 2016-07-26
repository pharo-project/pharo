initialization
whenSelectedItemChangesOnListModel

	criticsModel whenSelectedItemChanged: [ :selection |
		 sourceCodeModel setSelection: (0 to: 0). 	
		rule ifNotNil: [
			rule isArchitecturalRule ifFalse: [ "Architecural rules apply on Packages, nothing to display"
				(selection
					ifNil: [ self setTextModelForNil ]
					ifNotNil: [
						toolbarModel critic: selection.
						selection providesChange 
							ifTrue: [ self setTextModelForTransformationRule: selection ]
							ifFalse: [ self setTextModelForClassOrMethod: selection ] ] ) ]			
			]
		]