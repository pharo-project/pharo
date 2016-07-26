building
classesIn: a
	^ a list
		display: [ :object | 
			| all |
			all := OrderedCollection new.
			object class withAllSuperclassesDo: [:each | all add: each ].
			all ];
"		initialize: [ :list | list selection: list entity class ];"
		format: [ :class | class name ];
		morphicSelectionAct: [ :list | Smalltalk tools browser openOnClass: list selection ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			on: $b
			entitled: 'Browse'