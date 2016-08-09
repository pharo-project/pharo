initialization
whenSelectedItemChangesOnListModel

	criticsModel whenSelectedItemChanged: [ :critique |
		sourceCodeModel setSelection: (0 to: 0). 	
		rule ifNotNil: [
			critique
				ifNil: [ self setTextModelForNil ]
				ifNotNil: [  
					toolbarModel critic: critique.
					critique providesChange 
					ifTrue: [ self setTextModelForTransformationRule: critique ]
					ifFalse: [ self setTextModelForClassOrMethod: critique ] ] ] ]		